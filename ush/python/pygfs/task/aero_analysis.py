#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List, Any

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import add_to_datetime, to_fv3time, to_timedelta
from pygw.fsutils import rm_p, chdir
from pygw.timetools import to_fv3time
from pygw.yaml_file import YAMLFile, parse_yamltmpl, parse_j2yaml, save_as_yaml
from pygw.logger import logit
from pygw.executable import Executable
from pygw.exceptions import WorkflowException
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    @logit(logger, name="AerosolAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _res_enkf = int(self.config['CASE_ENKF'][1:])
        _window_begin = add_to_datetime(self.runtime_config.current_cycle, -to_timedelta(f"{self.config['assim_freq']}H") / 2)
        _fv3jedi_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.CDUMP}.t{self.runtime_config['cyc']:02d}z.aerovar.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'npx_anl': _res_enkf + 1,
                'npy_anl': _res_enkf + 1,
                'npz_anl': self.config['LEVS'] - 1,
                'AERO_WINDOW_BEGIN': _window_begin,
                'AERO_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'comin_ges_atm': self.config.COMIN_GES.replace('chem', 'atmos'),  # 'chem' is COMPONENT, aerosol fields are in 'atmos' tracers
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
        """Initialize a global aerosol analysis

        This method will initialize a global aerosol analysis using JEDI.
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
        crtm_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'aero_crtm_coeff.yaml')
        logger.debug(f"Staging CRTM fix files from {crtm_fix_list_path}")
        crtm_fix_list = parse_yamltmpl(crtm_fix_list_path, self.task_config)
        FileHandler(crtm_fix_list).sync()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'aero_jedi_fix.yaml')
        logger.debug(f"Staging JEDI fix files from {jedi_fix_list_path}")
        jedi_fix_list = parse_yamltmpl(jedi_fix_list_path, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage berror files
        # copy BUMP files, otherwise it will assume ID matrix
        if self.task_config.get('STATICB_TYPE', 'identity') in ['bump']:
            FileHandler(self.get_berror_dict(self.task_config)).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate variational YAML file
        logger.debug(f"Generate variational YAML file: {self.task_config.fv3jedi_yaml}")
        varda_yaml = parse_j2yaml(self.task_config['AEROVARYAML'], self.task_config)
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

        exec_cmd = Executable(self.task_config.APRUN_AEROANL)
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
        """Finalize a global aerosol analysis

        This method will finalize a global aerosol analysis using JEDI.
        This includes:
        - tarring up output diag files and place in ROTDIR
        - copying the generated YAML file from initialize to the ROTDIR
        - copying the guess files to the ROTDIR
        - applying the increments to the original RESTART files
        - moving the increment files to the ROTDIR

        Please note that some of these steps are temporary and will be modified
        once the model is able to read aerosol tracer increments.
        """
        # ---- tar up diags
        # path of output tar statfile
        aerostat = os.path.join(self.task_config['COMOUTaero'], f"{self.task_config['APREFIX']}aerostat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config['DATA'], 'diags', 'diag*nc4'))

        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(aerostat, "w") as archive:
            for diagfile in diags:
                archive.add(f"{diagfile}.gz")

        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.aerovar.yaml")
        dest = os.path.join(self.task_config['COMOUTaero'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.aerovar.yaml")
        yaml_copy = {
            'mkdir': [self.task_config['COMOUTaero']],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # ---- NOTE below is 'temporary', eventually we will not be using FMS RESTART formatted files
        # ---- all of the rest of this method will need to be changed but requires model and JEDI changes
        # ---- copy RESTART fv_tracer files for future reference
        template = '{}.fv_tracer.res.tile{}.nc'.format(to_fv3time(self.task_config.current_cycle), '{tilenum}')
        bkglist = []
        for itile in range(1, self.task_config.ntiles + 1):
            tracer = template.format(tilenum=itile)
            src = os.path.join(self.task_config.comin_ges_atm, 'RESTART', tracer)
            dest = os.path.join(self.task_config.COMOUTaero, f'aeroges.{tracer}')
            bkglist.append([src, dest])
        FileHandler({'copy': bkglist}).sync()

        # ---- add increments to RESTART files
        logger.info('Adding increments to RESTART files')
        self._add_fms_cube_sphere_increments()

        # ---- move increments to ROTDIR
        logger.info('Moving increments to ROTDIR')
        template = f'aeroinc.{to_fv3time(self.task_config.current_cycle)}.fv_tracer.res.tile{{tilenum}}'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            tracer = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'anl', tracer)
            dest = os.path.join(self.task_config.COMOUTaero, tracer)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

    def clean(self):
        super().clean()

    @logit(logger)
    def _add_fms_cube_sphere_increments(self: Analysis) -> None:
        """This method adds increments to RESTART files to get an analysis
        NOTE this is only needed for now because the model cannot read aerosol increments.
        This method will be assumed to be deprecated before this is implemented operationally
        """
        # only need the fv_tracer files
        template = '{}.fv_tracer.res.tile{}.nc'.format(to_fv3time(self.task_config.current_cycle), '{tilenum}')
        inc_template = os.path.join(self.task_config.DATA, 'anl', 'aeroinc.' + template)
        bkg_template = os.path.join(self.task_config.comin_ges_atm, 'RESTART', template)
        # get list of increment vars
        incvars_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'aeroanl_inc_vars.yaml')
        incvars = YAMLFile(path=incvars_list_path)['incvars']
        super().add_fv3_increments(inc_template, bkg_template, incvars)

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, core, tracer)
        that are needed for global aerosol DA and returns said dictionary for use by the FileHandler class.

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

        # aerosol DA needs coupler
        basename = f'{to_fv3time(task_config.current_cycle)}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        # aerosol DA only needs core/tracer
        for ftype in ['core', 'tracer']:
        template = f'{to_fv3time(self.task_config.current_cycle)}.fv_{ftype}.res.tile{{tilenum}}.nc'
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

        This method will construct a dictionary of BUMP background error files
        for global aerosol DA and return said dictionary for use by the FileHandler class.
        This dictionary contains coupler and fv_tracer files
        for correlation and standard deviation as well as NICAS localization.

        Parameters
        ----------
        config: Dict
            a dictionary containing all of the configuration needed

        Returns
        ----------
        berror_dict: Dict
            a dictionary containing the list of background error files to copy for FileHandler
        """
        # aerosol static-B needs nicas, cor_rh, cor_rv and stddev files.
        b_dir = config.BERROR_DATA_DIR
        b_datestr = to_fv3time(config.BERROR_DATE)
        berror_list = []

        for ftype in ['cor_rh', 'cor_rv', 'stddev']:
            coupler = f'{b_datestr}.{ftype}.coupler.res'
            berror_list.append([
                os.path.join(b_dir, coupler), os.path.join(config.DATA, 'berror', coupler)
            ])
            template = '{}.{}.fv_tracer.res.tile{}.nc'.format(b_datestr, ftype, '{tilenum}')
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
        berror_dict = {
            'mkdir': [os.path.join(config.DATA, 'berror')],
            'copy': berror_list,
        }
        return berror_dict
