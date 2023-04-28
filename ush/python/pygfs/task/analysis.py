#!/usr/bin/env python3

import os
from logging import getLogger
from netCDF4 import Dataset
from typing import List, Dict, Any

from pygw.yaml_file import YAMLFile, parse_j2yaml, parse_yamltmpl
from pygw.file_utils import FileHandler
from pygw.template import Template, TemplateConstants
from pygw.logger import logit
from pygw.task import Task
from pygw.timetools import to_fv3time, to_YMD

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

        # link jedi executable to run directory
        self.link_jediexe()

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
        logger.debug(f"OBS_LIST: {self.task_config['OBS_LIST']}")
        obs_list_config = parse_j2yaml(self.task_config["OBS_LIST"], self.task_config)
        logger.debug(f"obs_list_config: {obs_list_config}")
        # get observers from master dictionary
        observers = obs_list_config['observers']
        copylist = []
        for ob in observers:
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            copylist.append([os.path.join(self.task_config['COM_OBS'], basename), obfile])
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
        bias_dict: Dict
            a dictionary containing the list of observation bias files to copy for FileHandler
        """
        logger.debug(f"OBS_LIST: {self.task_config['OBS_LIST']}")
        obs_list_config = parse_j2yaml(self.task_config["OBS_LIST"], self.task_config)
        logger.debug(f"obs_list_config: {obs_list_config}")
        # get observers from master dictionary
        observers = obs_list_config['observers']
        copylist = []
        for ob in observers:
            if 'obs bias' in ob.keys():
                obfile = ob['obs bias']['input file']
                obdir = os.path.dirname(obfile)
                basename = os.path.basename(obfile)
                prefix = '.'.join(basename.split('.')[:-2])
                for file in ['satbias.nc4', 'satbias_cov.nc4', 'tlapse.txt']:
                    bfile = f"{prefix}.{file}"
                    copylist.append([os.path.join(self.task_config.COM_ATMOS_ANALYSIS_PREV, bfile), os.path.join(obdir, bfile)])

        bias_dict = {
            'mkdir': [os.path.join(self.runtime_config.DATA, 'bc')],
            'copy': copylist
        }
        return bias_dict

    @logit(logger)
    def add_fv3_increments(self, inc_file_tmpl: str, bkg_file_tmpl: str, incvars: List) -> None:
        """Add cubed-sphere increments to cubed-sphere backgrounds

        Parameters
        ----------
        inc_file_tmpl : str
           template of the FV3 increment file of the form: 'filetype.tile{tilenum}.nc'
        bkg_file_tmpl : str
           template of the FV3 background file of the form: 'filetype.tile{tilenum}.nc'
        incvars : List
           List of increment variables to add to the background
        """

        for itile in range(1, self.config.ntiles + 1):
            inc_path = inc_file_tmpl.format(tilenum=itile)
            bkg_path = bkg_file_tmpl.format(tilenum=itile)
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
    def link_jediexe(self: Task) -> None:
        """Compile a dictionary of background error files to copy

        This method links a JEDI executable to the run directory

        Parameters
        ----------
        Task: GDAS task

        Returns
        ----------
        None
        """
        exe_src = self.task_config.JEDIEXE

        # TODO: linking is not permitted per EE2.  Needs work in JEDI to be able to copy the exec.
        logger.debug(f"Link executable {exe_src} to DATA/")
        exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        return

    @logit(logger)
    def get_bkg_ens_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of ensemble FV3 restart files (coupler, core, tracer)
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
        # NOTE for now this is FV3 restart files and just assumed to be fh006
        # loop over ensemble members
        rstlist = []
        dirlist = []
        bkglist = []
        for imem in range(1, self.task_config.NMEM_ENKF + 1):
            memchar = f"mem{imem:03d}"

            # accumulate directory list for member restart files
            dirlist.append(os.path.join(self.task_config.DATA, 'bkg', memchar))

            # get FV3 restart files, this will be a lot simpler when using history files
            template = self.task_config.COM_ATMOS_RESTART_TMPL
            tmpl_dict = {
                'ROTDIR': self.task_config.ROTDIR,
                'RUN': self.runtime_config.RUN,
                'YMD': to_YMD(self.task_config.previous_cycle),
                'HH': self.task_config.previous_cycle.strftime('%H')
            }

            # get FV3 restart files, this will be a lot simpler when using history files
            tmpl_dict['MEMDIR'] = memchar
            rst_dir = Template.substitute_structure(template, TemplateConstants.DOLLAR_CURLY_BRACE, tmpl_dict.get)
            rstlist.append(rst_dir)

            run_dir = os.path.join(self.task_config.DATA, 'bkg', memchar)

            # atmens DA needs coupler
            basename = f'{to_fv3time(self.task_config.current_cycle)}.coupler.res'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(self.task_config.DATA, 'bkg', memchar, basename)])

            # atmens DA needs core, srf_wnd, tracer, phy_data, sfc_data
            for ftype in ['fv_core.res', 'fv_srf_wnd.res', 'fv_tracer.res', 'phy_data', 'sfc_data']:
                template = f'{to_fv3time(self.task_config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
                for itile in range(1, self.task_config.ntiles + 1):
                    basename = template.format(tilenum=itile)
                    bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': rstlist,
            'mkdir': dirlist,
            'copy': bkglist,
        }

        return bkg_dict
