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
from pygw.timetools import to_isotime, to_fv3time, to_timedelta
from pygw.fsutils import rm_p
from pygw.template import Template, TemplateConstants
from pygw.yaml_file import YAMLFile
from pygw.logger import logit
from pygw.executable import Executable
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AtmEnsAnalysis(Analysis):
    """
    Class for global atmens analysis tasks
    """
    @logit(logger, name="AtmEnsAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config['CASE_ANL'][1:])
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
                'comin_ges_atmens': self.config.COMIN_GES_ENS,
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """Initialize a global atmens analysis

        This method will initialize a global atmens analysis using JEDI.
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
        crtm_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'atm_crtm_coeff.yaml')
        logger.debug(f"Staging CRTM fix files from {crtm_fix_list_path}")
        crtm_fix_list = parse_yamltmpl(crtm_fix_list_path, self.task_config)
        FileHandler(crtm_fix_list).sync()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'atm_jedi_fix.yaml')
        logger.debug(f"Staging JEDI fix files from {jedi_fix_list_path}")
        jedi_fix_list = parse_yamltmpl(jedi_fix_list_path, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate variational YAML file
        yaml_out = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atmens.yaml")
        varda_yaml = YAMLFile(path=self.task_config['ATMENSYAML'])
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOUBLE_CURLY_BRACES, self.task_config.get)
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        varda_yaml.save(yaml_out)
        logger.info(f"Wrote YAML to {yaml_out}")

        # link var executable
        exe_src = self.task_config['JEDIENSEXE']
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

        # Make directories for member analsis files
        for imem in range(1, self.task_config['NMEM_ENKF'] + 1):
            memchar = f"mem{imem:03d}"
            anldir = [
                os.path.join(self.task_config['DATA'], 'anl', memchar)
            ]
            FileHandler({'mkdir': anldir}).sync()

    @logit(logger)
    def finalize(self: Analysis) -> None:
        """Finalize a global atmens analysis

        This method will finalize a global atmens analysis using JEDI.
        This includes:
        - tarring up output diag files and place in ROTDIR
        - copying the generated YAML file from initialize to the ROTDIR
        - rewriting UFS-DA increments to UFS model readable format with delp and hydrostatic delz calculation

        """
        # ---- tar up diags
        # path of output tar statfile
        atmensstat = os.path.join(self.task_config['COMOUT'], f"{self.task_config['APREFIX']}atmensstat")

        # get list of diag files to put in tarball
        diags = glob.glob('diags/diag*nc4')

        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(atmensstat, "w") as archive:
            for diagfile in diags:
                archive.add(f"{diagfile}.gz")

        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atmens.yaml")
        dest = os.path.join(self.task_config['COMOUT'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atmens.yaml")
        yaml_copy = {
            'mkdir': [self.task_config['COMOUT']],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # rewrite UFS-DA atmens increments to UFS model readable format with delp and hydrostatic delz calculation
        gprefix = self.task_config['GPREFIX']
        cdate_inc = self.task_config.cdate_fv3.replace('.', '_')
        incpy = os.path.join(self.task_config['HOMEgfs'], 'sorc/gdas.cd/ush/jediinc2fv3.py')

        for imem in range(1, self.task_config['NMEM_ENKF'] + 1):
            memchar = f"mem{imem:03d}"

            # make output directory for member increment
            incdir = [
                os.path.join(self.task_config['COMOUT'], memchar, 'atmos')
            ]
            FileHandler({'mkdir': incdir}).sync()

            # rewrite UFS-DA atmens increments
            atmges_fv3 = os.path.join(self.task_config['COMIN_GES_ENS'], memchar, 'atmos',
                                      f"{self.task_config['CDUMP']}.t{self.task_config['gcyc']:02d}z.atmf006.nc")
            atminc_jedi = os.path.join(self.task_config['DATA'], 'anl', memchar, f'atminc.{cdate_inc}z.nc4')
            atminc_fv3 = os.path.join(self.task_config['COMOUT'], memchar, 'atmos',
                                      f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.atminc.nc")

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
        that are needed for global atmens DA and returns said dictionary for use by the FileHandler class.

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

        bkgdir = [
            os.path.join(task_config['DATA'], 'bkg'),
        ]
        FileHandler({'mkdir': bkgdir}).sync()

        # loop over ensemble members
        bkglist = []
        for imem in range(1, task_config['NMEM_ENKF'] + 1):
            memchar = f"mem{imem:03d}"

            # make run directory for member restart files
            bkgdir = [
                os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART')
            ]
            FileHandler({'mkdir': bkgdir}).sync()

            # get FV3 RESTART files, this will be a lot simpler when using history files
            rst_dir = os.path.join(task_config.comin_ges_atmens, memchar, 'atmos/RESTART')

            basename = f'{task_config.cdate_fv3}.coupler.res'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])
            basename = f'{task_config.cdate_fv3}.fv_core.res.nc'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])
            basename_cadat = f'{task_config.cdate_fv3}.ca_data.tileX.nc'
            basename_core = f'{task_config.cdate_fv3}.fv_core.res.tileX.nc'
            basename_srfwnd = f'{task_config.cdate_fv3}.fv_srf_wnd.res.tileX.nc'
            basename_tracer = f'{task_config.cdate_fv3}.fv_tracer.res.tileX.nc'
            basename_phydat = f'{task_config.cdate_fv3}.phy_data.tileX.nc'
            basename_sfcdat = f'{task_config.cdate_fv3}.sfc_data.tileX.nc'
            for itile in range(1, task_config.ntiles + 1):
                basename = basename_cadat.replace('tileX', f'tile{itile}')
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])
                basename = basename_core.replace('tileX', f'tile{itile}')
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])
                basename = basename_srfwnd.replace('tileX', f'tile{itile}')
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])
                basename = basename_tracer.replace('tileX', f'tile{itile}')
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])
                basename = basename_phydat.replace('tileX', f'tile{itile}')
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])
                basename = basename_sfcdat.replace('tileX', f'tile{itile}')
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', memchar, 'RESTART', basename)])

        bkg_dict = {
            'copy': bkglist,
        }
        return bkg_dict
