#!/usr/bin/env python3

import os
from logging import getLogger
from netCDF4 import Dataset
from typing import List, Dict, Any

from pygw.yaml_file import YAMLFile
from pygw.file_utils import FileHandler
from pygw.logger import logit
from pygw.task import Task
from pygw.template import Template, TemplateConstants

logger = getLogger(__name__.split('.')[-1])


class Analysis(Task):
    """Parent class for GDAS tasks

    The Analysis class is the parent class for all
    Global Data Assimilation System (GDAS) tasks
    directly related to peforming an analysis
    """

    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)
        self.config.ntiles = 6

    def initialize(self) -> None:
        super().initialize()
        # all analyses need to stage observations
        obs_dict = self.get_obs_dict()
        FileHandler(obs_dict).sync()

        # some analyses need to stage bias corrections
        bias_dict = self.get_bias_dict()
        FileHandler(bias_dict).sync()

    @logit(logger)
    def get_obs_dict(self: Task) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method uses the OBS_LIST configuration variable to generate a dictionary
        from a list of YAML files that specify what observation files are to be
        copied to the run directory from the observation input directory

        Parameters
        ----------

        Returns
        ----------
        obs_dict: Dict
            a dictionary containing the list of observation files to copy for FileHandler
        """
        obs_list_config = YAMLFile(path=self.config['OBS_LIST'])

        # get observers from master dictionary
        observers = obs_list_config['observers']
        copylist = []
        for ob in observers:
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            copylist.append([os.path.join(self.config['COMIN_OBS'], basename), obfile])
        obs_dict = {
            'mkdir': [os.path.join(self.runtime_config['DATA'], 'obs')],
            'copy': copylist
        }
        return obs_dict

    @logit(logger)
    def get_bias_dict(self: Task) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method uses the OBS_LIST configuration variable to generate a dictionary
        from a list of YAML files that specify what observation bias correction files
        are to be copied to the run directory from the observation input directory

        Parameters
        ----------

        Returns
        ----------
        obs_dict: Dict
            a dictionary containing the list of observation bias files to copy for FileHandler
        """
        obs_list_config = YAMLFile(path=self.config['OBS_LIST'])
        # get observers from master dictionary
        observers = obs_list_config['observers']
        copylist = []
        for ob in observers:
            if 'obs bias' in ob.keys():
                obfile = ob['obs bias']['input file']
                for obfile in ['satbias.nc4', 'satbias_cov.nc4', 'tlapse.txt']:
                    basename = os.path.basename(obfile)
                    copylist.append([os.path.join(self.task_config.comin_ges_atm, basename), obfile])

        obs_dict = {
            'mkdir': [os.path.join(self.runtime_config['DATA'], 'bc')],
            'copy': copylist
        }
        return obs_dict

    @logit(logger)
    def add_fv3_increments(self, inc_file_tmpl: str, bkg_file_tmpl: str, incvars: List) -> None:
        """Add cubed-sphere increments to cubed-sphere backgrounds

        Parameters
        ----------
        inc_file_tmpl : str
           template of the FV3 increment file of the form: 'filetype.{tileX}.nc'
        bkg_file_tmpl : str
           template of the FV3 background file of the form: 'filetype.{tileX}.nc'
        incvars : List
           List of increment variables to add to the background
        """

        for tt in range(1, self.config.ntiles + 1):
            inc_path = inc_file_tmpl.replace('tileX', f'tile{tt}')
            bkg_path = bkg_file_tmpl.replace('tileX', f'tile{tt}')
            with Dataset(inc_path, mode='r') as incfile, Dataset(bkg_path, mode='a') as rstfile:
                for vname in incvars:
                    increment = incfile.variables[vname][:]
                    bkg = rstfile.variables[vname][:]
                    anl = bkg + increment
                    rstfile.variables[vname][:] = anl[:]
                    try:
                        rstfile.variables[vname].delncattr('checksum')  # remove the checksum so fv3 does not complain
                    except (AttributeError, RuntimeError):
                        pass  # checksum is missing, move on

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method is a placeholder for now... will be possibly made generic at a later date

        Parameters
        ----------
        task_config: Dict
            a dictionary containing all of the configuration needed for the task

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        bkg_dict = {'foo': 'bar'}
        return bkg_dict

    @logit(logger)
    def get_berror_dict(self, config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of background error files to copy

        This method is a placeholder for now... will be possibly made generic at a later date

        Parameters
        ----------
        config: Dict
            a dictionary containing all of the configuration needed

        Returns
        ----------
        berror_dict: Dict
            a dictionary containing the list of background error files to copy for FileHandler
        """
        berror_dict = {'foo': 'bar'}
        return berror_dict

    @logit(logger)
    def stage_fix(self, fix_yaml: str) -> None:
        """Stage fix files

        This method is a placeholder for now... will be possibly made generic at a later date

        Parameters
        ----------
        fix_yaml : str
            tamplate of fix files to copy
        """
        fix_list_path = os.path.join(self.config['HOMEgfs'], 'parm', 'parm_gdas', fix_yaml)
        fix_list = YAMLFile(path=fix_list_path)
        fix_list = Template.substitute_structure(fix_list, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        FileHandler(fix_list).sync()
