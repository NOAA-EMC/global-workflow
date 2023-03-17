#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from netCDF4 import Dataset
from logging import getLogger
from typing import Dict, List, Any

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import to_isotime, to_fv3time, to_timedelta, datetime_to_YMDH
from pygw.fsutils import rm_p
from pygw.template import Template, TemplateConstants
from pygw.yaml_file import YAMLFile
from pygw.logger import logit
from pygw.executable import Executable
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
        _res_enkf = int(self.config['CASE_ENKF'][1:])
        _window_begin = self.runtime_config.current_cycle - to_timedelta(f"{self.config['assim_freq']}H") / 2

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
                'ATM_WINDOW_BEGIN': to_isotime(_window_begin),
                'ATM_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'BKG_ISOTIME': to_isotime(self.runtime_config.current_cycle),
                'BKG_YYYYmmddHHMMSS': to_fv3time(self.runtime_config.current_cycle),
                'cdate_fv3': to_fv3time(self.runtime_config.current_cycle),
                'comin_ges_atm': self.config.COMIN_GES,
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

        # stage observations and bias corrections
        super().initialize()

        # stage CRTM fix files
        super().stage_fix('atm_crtm_coeff.yaml')

        # stage fix files
        super().stage_fix('atm_jedi_fix.yaml')

        # stage berror files
        # copy static background error files, otherwise it will assume ID matrix
        if self.task_config.get('STATICB_TYPE', 'identity') in ['gsibec']:
            FileHandler(self.get_berror_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate variational YAML file
        yaml_out = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atmvar.yaml")
        varda_yaml = YAMLFile(path=self.task_config['ATMVARYAML'])
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOUBLE_CURLY_BRACES, self.task_config.get)
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        varda_yaml.save(yaml_out)
        logger.info(f"Wrote YAML to {yaml_out}")

        # link var executable
        exe_src = self.task_config['JEDIVAREXE']
        exe_dest = os.path.join(self.task_config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # need output dir for diags and anl
        newdirs = [
            os.path.join(self.task_config['DATA'], 'anl'),
            os.path.join(self.task_config['DATA'], 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def finalize(self: Analysis) -> None:
        """Finalize a global atm analysis

        This method will finalize a global atm analysis using JEDI.
        This includes:
        - tarring up output diag files and place in ROTDIR
        - copying the generated YAML file from initialize to the ROTDIR
        - copying the update bias correction files to ROTDIR
        - rewrite UFS-DA atm increment to UFS model readable format with delp and hydrostatic delz calculation

        Please note that some of these steps are temporary and will be modified
        once the model is able to read atm increments.
        """
        # ---- tar up diags
        # path of output tar statfile
        atmstat = os.path.join(self.task_config['COMOUTatmos'], f"{self.task_config['APREFIX']}atmstat")

        # get list of diag files to put in tarball
        diags = glob.glob('diags/diag*nc4')

        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(atmstat, "w") as archive:
            for diagfile in diags:
                archive.add(f"{diagfile}.gz")

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
        gdate = datetime_to_YMDH(self.task_config['GDATE'])
        gsuffix = f"{gdate}" + ".txt"
        aprefix = f"{self.task_config['APREFIX']}"
        adate = datetime_to_YMDH(self.task_config['CDATE'])
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
        gprefix = self.task_config['GPREFIX']
        if case_berror == case:
            atmges_fv3 = os.path.join(self.task_config.comin_ges_atm, f"{self.task_config['GPREFIX']}atmf006.nc")
        else:
            atmges_fv3 = os.path.join(self.task_config.comin_ges_atm, f"{self.task_config['GPREFIX']}atmf006.ensres.nc")

        cdate_inc = self.task_config.cdate_fv3.replace('.', '_')
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
        super().get_bkg_dict(task_config)
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 RESTART files, this will be a lot simpler when using history files
        rst_dir = os.path.join(task_config.comin_ges_atm, 'RESTART')  # for now, option later?

        bkglist = []
        basename = f'{task_config.cdate_fv3}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
        basename = f'{task_config.cdate_fv3}.fv_core.res.nc'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
        basename_cadat = f'{task_config.cdate_fv3}.ca_data.tileX.nc'
        basename_core = f'{task_config.cdate_fv3}.fv_core.res.tileX.nc'
        basename_srfwnd = f'{task_config.cdate_fv3}.fv_srf_wnd.res.tileX.nc'
        basename_tracer = f'{task_config.cdate_fv3}.fv_tracer.res.tileX.nc'
        basename_phydat = f'{task_config.cdate_fv3}.phy_data.tileX.nc'
        basename_sfcdat = f'{task_config.cdate_fv3}.sfc_data.tileX.nc'
        for itile in range(1, task_config.ntiles + 1):
            basename = basename_cadat.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
            basename = basename_core.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
            basename = basename_srfwnd.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
            basename = basename_tracer.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
            basename = basename_phydat.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
            basename = basename_sfcdat.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
        bkg_dict = {
            'mkdir': [os.path.join(task_config['DATA'], 'bkg')],
            'copy': bkglist,
        }
        return bkg_dict

    @logit(logger)
    def get_berror_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
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
        super().get_berror_dict(task_config)

        berror_list = []
        for ftype in ['gfs_gsi_global.nml', 'gsi-coeffs-gfs-global.nc4']:
            berror_list.append([
                os.path.join(task_config['FV3JEDI_FIX'], 'gsibec', task_config['CASE_ANL'], ftype),
                os.path.join(task_config['DATA'], 'berror', ftype)
            ])
        berror_dict = {
            'mkdir': [os.path.join(task_config['DATA'], 'berror')],
            'copy': berror_list,
        }
        return berror_dict
