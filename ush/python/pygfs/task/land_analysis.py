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
from pygfs.task.analysis import Analysis
from datetime import date

logger = getLogger(__name__.split('.')[-1])


class LandAnalysis(Analysis):
    """
    Class for global land analysis tasks
    """
    @logit(logger, name="LandAnalysis")
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
                'LAND_WINDOW_BEGIN': to_isotime(_window_begin),
                'LAND_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'LAND_BKG_ISOTIME': to_isotime(self.runtime_config.current_cycle),
                'LAND_BKG_YYYYmmddHHMMSS': to_fv3time(self.runtime_config.current_cycle),
                'cdate_fv3': to_fv3time(self.runtime_config.current_cycle),
                'comin_ges_atm': self.config.COMIN_GES.replace('chem', 'atmos'),  # 'chem' is COMPONENT, land fields are in 'atmos' sfc
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """Initialize a global land analysis

        This method will initialize a global land analysis using JEDI.
        This includes:
        #- staging CRTM fix files
        - staging FV3-JEDI fix files
        #- staging B error files
        - staging model backgrounds
        - generating a YAML file for the JEDI executable
        - linking the JEDI executable (TODO make it copyable, requires JEDI fix)
        - creating output directories
        """
        super().initialize()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'land_jedi_fix.yaml')
        jedi_fix_list = YAMLFile(path=jedi_fix_list_path)
        jedi_fix_list = Template.substitute_structure(jedi_fix_list, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate letkfoi YAML file
        yaml_out = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.landoi.yaml")
        letkfoi_yaml = YAMLFile(path=self.task_config['LANDVARYAML'])
        letkfoi_yaml = Template.substitute_structure(letkfoi_yaml, TemplateConstants.DOUBLE_CURLY_BRACES, self.task_config.get)
        letkfoi_yaml = Template.substitute_structure(letkfoi_yaml, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        letkfoi_yaml.save(yaml_out)
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
        """Finalize a global land analysis

        This method will finalize a global land analysis using JEDI.
        This includes:
        - tarring up output diag files and place in ROTDIR
        - copying the generated YAML file from initialize to the ROTDIR
        - copying the guess files to the ROTDIR
        - applying the increments to the original RESTART files
        - moving the increment files to the ROTDIR

        Please note that some of these steps are temporary and will be modified
        once the model is able to read land increments.
        """
        # ---- tar up diags
        # path of output tar statfile
        landstat = os.path.join(self.task_config['COMOUTatmos'], f"{self.task_config['APREFIX']}landstat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config['DATA'], 'diags', 'diag*nc4'))

        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(landstat, "w") as archive:
            for diagfile in diags:
                archive.add(f"{diagfile}.gz")

        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.landoi.yaml")
        dest = os.path.join(self.task_config['COMOUTatmos'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.landoi.yaml")
        yaml_copy = {
            'mkdir': [self.task_config['COMOUTatmos']],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # ---- NOTE below is 'temporary', eventually we will not be using FMS RESTART formatted files
        # ---- all of the rest of this method will need to be changed but requires model and JEDI changes
        # ---- copy RESTART sfc_data files for future reference
        fms_bkg_file_template = os.path.join(self.task_config.comin_ges_atm, 'RESTART', f'{self.task_config.cdate_fv3}.sfc_data.tileX.nc')
        bkglist = []
        for itile in range(1, self.task_config.ntiles + 1):
            bkg_path = fms_bkg_file_template.replace('tileX', f'tile{itile}')
            dest = os.path.join(self.task_config['COMOUTatmos'], f'landges.{os.path.basename(bkg_path)}')
            bkglist.append([bkg_path, dest])
        FileHandler({'copy': bkglist}).sync()

        # ---- add increments to RESTART files
        #logger.info('Adding increments to RESTART files')
        #self._add_fms_cube_sphere_increments()

        # ---- move increments to ROTDIR
        logger.info('Moving increments to ROTDIR')
        fms_inc_file_template = os.path.join(self.task_config['DATA'], 'anl', f'landinc.{self.task_config.cdate_fv3}.sfc_data.tileX.nc')
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            inc_path = fms_inc_file_template.replace('tileX', f'tile{itile}')
            dest = os.path.join(self.task_config['COMOUTatmos'], os.path.basename(inc_path))
            inclist.append([inc_path, dest])
        FileHandler({'copy': inclist}).sync()

        # ---- move analysis to ROTDIR/RESTART
        logger.info('Moving analysis to ROTDIR')
        fms_anl_file_template = os.path.join(self.task_config['DATA'], 'anl', f'{self.task_config.cdate_fv3}.sfcanl_data.tileX.nc')
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            inc_path = fms_anl_file_template.replace('tileX', f'tile{itile}')
#DONG            dest = os.path.join(self.task_config['COMOUTatmos'], 'RESTART', f'{os.path.basename(inc_path)}')
            dest = os.path.join(self.task_config['COMOUTatmos'], f'{os.path.basename(inc_path)}')
            inclist.append([inc_path, dest])
        FileHandler({'copy': inclist}).sync()

    def clean(self):
        super().clean()

    @logit(logger)
    def _add_fms_cube_sphere_increments(self: Analysis) -> None:
        """This method adds increments to RESTART files to get an analysis
        NOTE this is only needed for now because the model cannot read land increments.
        This method will be assumed to be deprecated before this is implemented operationally
        """
        # only need the sfc_data files
        fms_inc_file_template = os.path.join(self.task_config['DATA'], 'anl', f'landinc.{self.task_config.cdate_fv3}.sfc_data.tileX.nc')
        fms_bkg_file_template = os.path.join(self.task_config.comin_ges_atm, 'RESTART', f'{self.task_config.cdate_fv3}.sfc_data.tileX.nc')
        # get list of increment vars
        incvars_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'landanl_inc_vars.yaml')
        incvars = YAMLFile(path=incvars_list_path)['incvars']
        super().add_fv3_increments(fms_inc_file_template, fms_bkg_file_template, incvars)

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, core, sfc)
        that are needed for global land DA and returns said dictionary for use by the FileHandler class.

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

        # land DA only needs core/sfc_data
        bkglist = []
        basename = f'{task_config.cdate_fv3}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
        basename_core = f'{task_config.cdate_fv3}.fv_core.res.tileX.nc'
        basename_sfc = f'{task_config.cdate_fv3}.sfc_data.tileX.nc'
        for itile in range(1, task_config.ntiles + 1):
            basename = basename_core.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
            basename = basename_sfc.replace('tileX', f'tile{itile}')
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
        bkg_dict = {
            'mkdir': [os.path.join(task_config['DATA'], 'bkg')],
            'copy': bkglist,
        }
        return bkg_dict

