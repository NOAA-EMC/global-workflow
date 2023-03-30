#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List, Any

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import add_to_datetime, to_fv3time, to_timedelta, to_YMDH
from pygw.fsutils import rm_p, chdir
from pygw.yaml_file import YAMLFile, parse_yamltmpl, parse_j2yaml, save_as_yaml
from pygw.logger import logit
from pygw.executable import Executable
from pygw.exceptions import WorkflowException
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AtmAnalysis(Analysis):
    """
    Class for global atm analysis tasks
    """
    @logit(logger, name="AtmAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _res_anl = int(self.config['CASE_ANL'][1:])
        _window_begin = add_to_datetime(self.runtime_config.current_cycle, -to_timedelta(f"{self.config['assim_freq']}H") / 2)
        _fv3jedi_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.CDUMP}.t{self.runtime_config['cyc']:02d}z.atmvar.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.config['LEVS'] - 1,
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'comin_ges_atm': self.config.COMIN_GES,
                'OPREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'APREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'GPREFIX': f"gdas.t{self.runtime_config.previous_cycle.hour:02d}z.",
                'fv3jedi_yaml': _fv3jedi_yaml,
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """Initialize a global atm analysis

        This method will initialize a global atm analysis using JEDI.
        This includes:
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging B error files
        - staging model backgrounds
        - generating a YAML file for the JEDI executable
        - linking the JEDI executable (TODO make it copyable, requires JEDI fix)
        - creating output directories
        """
        super().initialize()

        # stage CRTM fix files
        crtm_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'atm_crtm_coeff.yaml')
        logger.debug(f"Staging CRTM fix files from {crtm_fix_list_path}")
        crtm_fix_list = parse_yamltmpl(crtm_fix_list_path, self.task_config)
        FileHandler(crtm_fix_list).sync()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'atm_jedi_fix.yaml')
        logger.debug(f"Staging JEDI fix files from {jedi_fix_list_path}")
        jedi_fix_list = parse_yamltmpl(jedi_fix_list_path, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage berror files
        # copy static background error files, otherwise it will assume ID matrix
        if self.task_config.get('STATICB_TYPE', 'identity') in ['bump', 'gsibec']:
            FileHandler(self.get_berror_dict(self.task_config)).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate variational YAML file
        logger.debug(f"Generate variational YAML file: {self.task_config.fv3jedi_yaml}")
        varda_yaml = parse_j2yaml(self.task_config['ATMVARYAML'], self.task_config)
        save_as_yaml(varda_yaml, self.task_config.fv3jedi_yaml)
        logger.info(f"Wrote variational YAML to: {self.task_config.fv3jedi_yaml}")

        # link executable to DATA/ directory
        exe_src = self.task_config['JEDIVAREXE']
        logger.debug(f"Link executable {exe_src} to DATA/")  # TODO: linking is not permitted per EE2.  Needs work in JEDI to be able to copy the exec.
        exe_dest = os.path.join(self.task_config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # need output dir for diags and anl
        logger.debug("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config['DATA'], 'anl'),
            os.path.join(self.task_config['DATA'], 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def execute(self: Analysis) -> None:

        chdir(self.task_config.DATA)

        exec_cmd = Executable(self.task_config.APRUN_ATMANL)
        exec_name = os.path.join(self.task_config.DATA, 'fv3jedi_var.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg(self.task_config.fv3jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def finalize(self: Analysis) -> None:
        """Finalize a global atm analysis

        This method will finalize a global atm analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - copy the updated bias correction files to ROTDIR
        - rewrite UFS-DA atm increment to UFS model readable format with delp and hydrostatic delz calculation

        Please note that some of these steps are temporary and will be modified
        once the model is able to read atm increments.
        """
        # ---- tar up diags
        # path of output tar statfile
        atmstat = os.path.join(self.task_config['COMOUTatmos'], f"{self.task_config['APREFIX']}atmstat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config['DATA'], 'diags', 'diag*nc4'))

        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(atmstat, "w") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atmvar.yaml")
        dest = os.path.join(self.task_config['COMOUTatmos'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atmvar.yaml")
        yaml_copy = {
            'mkdir': [self.task_config['COMOUTatmos']],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # copy bias correction files to ROTDIR
        biasdir = os.path.join(self.task_config['DATA'], 'bc')
        biasls = os.listdir(biasdir)
        biaslist = []
        for bfile in biasls:
            src = os.path.join(biasdir, bfile)
            dest = os.path.join(self.task_config['COMOUTatmos'], bfile)
            biaslist.append([src, dest])

        gprefix = f"{self.task_config['GPREFIX']}"
        gdate = to_YMDH(self.task_config.previous_cycle)
        gsuffix = f"{gdate}" + ".txt"
        aprefix = f"{self.task_config['APREFIX']}"
        adate = to_YMDH(self.task_config.current_cycle)
        asuffix = f"{adate}" + ".txt"

        obsdir = os.path.join(self.task_config['DATA'], 'obs')
        obsls = os.listdir(obsdir)
        for ofile in obsls:
            if ofile.endswith(".txt"):
                src = os.path.join(obsdir, ofile)
                tfile = ofile.replace(gprefix, aprefix)
                tfile = tfile.replace(gsuffix, asuffix)
                dest = os.path.join(self.task_config['COMOUTatmos'], tfile)
                biaslist.append([src, dest])

        bias_copy = {
            'mkdir': [self.task_config['COMOUTatmos']],
            'copy': biaslist,
        }
        FileHandler(bias_copy).sync()

        # rewrite UFS-DA atm increment to UFS model readable format with delp and hydrostatic delz calculation
        case_berror = int(self.config['CASE_ANL'][1:])
        case = int(self.config['CASE'][1:])
        if case_berror == case:
            atmges_fv3 = os.path.join(self.task_config.comin_ges_atm, f"{self.task_config['GPREFIX']}atmf006.nc")
        else:
            atmges_fv3 = os.path.join(self.task_config.comin_ges_atm, f"{self.task_config['GPREFIX']}atmf006.ensres.nc")

        cdate = to_fv3time(self.task_config.current_cycle)
        cdate_inc = cdate.replace('.', '_')
        atminc_jedi = os.path.join(self.task_config['DATA'], 'anl', f'atminc.{cdate_inc}z.nc4')
        atminc_fv3 = os.path.join(self.task_config['COMOUTatmos'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atminc.nc")
        incpy = os.path.join(self.task_config['HOMEgfs'], 'sorc/gdas.cd/ush/jediinc2fv3.py')

        cmd = Executable(incpy)
        cmd.add_default_arg(atmges_fv3)
        cmd.add_default_arg(atminc_jedi)
        cmd.add_default_arg(atminc_fv3)
        cmd(output='stdout', error='stderr')

    def clean(self):
        super().clean()

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, core, tracer)
        that are needed for global atm DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        task_config: Dict
            a dictionary containing all of the configuration needed for the task

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 RESTART files, this will be a lot simpler when using history files
        rst_dir = os.path.join(task_config.comin_ges_atm, 'RESTART')  # for now, option later?
        run_dir = os.path.join(task_config['DATA'], 'bkg')

        # Start accumulating list of background files to copy
        bkglist = []

        # atm DA needs coupler
        basename = f'{to_fv3time(task_config.current_cycle)}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        # atm DA needs core, srf_wnd, tracer, phy_data, sfc_data
        for ftype in ['core', 'srf_wnd', 'tracer']:
            template = f'{to_fv3time(self.task_config.current_cycle)}.fv_{ftype}.res.tile{{tilenum}}.nc'
            for itile in range(1, task_config.ntiles + 1):
                basename = template.format(tilenum=itile)
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])
        for ftype in ['phy_data', 'sfc_data']:
            template = f'{to_fv3time(self.task_config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
            for itile in range(1, task_config.ntiles + 1):
                basename = template.format(tilenum=itile)
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': [run_dir],
            'copy': bkglist,
        }
        return bkg_dict

    @logit(logger)
    def get_berror_dict(self, config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of background error files to copy

        This method will construct a dictionary of background error files
        for global atm DA and return said dictionary for use by the FileHandler class.

        Parameters
        ----------
        config: Dict
            a dictionary containing all of the configuration needed

        Returns
        ----------
        berror_dict: Dict
            a dictionary containing the list of atm background error files to copy for FileHandler
        """
        if config.STATICB_TYPE in ['bump']:
            # BUMP atm static-B needs nicas, cor_rh, cor_rv and stddev files.
            b_dir = config.BERROR_DATA_DIR
            b_datestr = to_fv3time(config.BERROR_DATE)
            berror_list = []

            for ftype in ['cor_rh', 'cor_rv', 'stddev']:
                coupler = f'{b_datestr}.{ftype}.coupler.res'
                berror_list.append([
                    os.path.join(b_dir, coupler), os.path.join(config.DATA, 'berror', coupler)
                ])
                template = '{b_datestr}.{ftype}.fv_tracer.res.tile{{tilenum}}.nc'
                for itile in range(1, config.ntiles + 1):
                    tracer = template.format(tilenum=itile)
                    berror_list.append([
                        os.path.join(b_dir, tracer), os.path.join(config.DATA, 'berror', tracer)
                    ])

            nproc = config.ntiles * config.layout_x * config.layout_y
            for nn in range(1, nproc + 1):
                berror_list.append([
                    os.path.join(b_dir, f'nicas_aero_nicas_local_{nproc:06}-{nn:06}.nc'),
                    os.path.join(config.DATA, 'berror', f'nicas_aero_nicas_local_{nproc:06}-{nn:06}.nc')
                ])
        else:
            # GSI atm static-B needs namelist and coefficient files.
            b_dir = os.path.join(config.FV3JEDI_FIX, 'gsibec', config.CASE_ANL)
            berror_list = []
            for ftype in ['gfs_gsi_global.nml', 'gsi-coeffs-gfs-global.nc4']:
                berror_list.append([
                    os.path.join(b_dir, ftype),
                    os.path.join(config.DATA, 'berror', ftype)
                ])

        # stage background error files
        berror_dict = {
            'mkdir': [os.path.join(config.DATA, 'berror')],
            'copy': berror_list,
        }
        return berror_dict
