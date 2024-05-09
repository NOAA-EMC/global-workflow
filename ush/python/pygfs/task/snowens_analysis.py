#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List
from pprint import pformat
import numpy as np
from netCDF4 import Dataset

from wxflow import (AttrDict,
                    FileHandler,
                    to_fv3time, to_YMD, to_YMDH, to_timedelta, add_to_datetime,
                    rm_p,
                    parse_j2yaml, save_as_yaml,
                    Jinja,
                    logit,
                    Executable,
                    WorkflowException)
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class SnowEnsAnalysis(Analysis):
    """
    Class for global ensemble snow analysis tasks
    """

    NMEM_SNOWENS = 2

    @logit(logger, name="SnowEnsAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _window_begin = add_to_datetime(self.runtime_config.current_cycle, -to_timedelta(f"{self.config['assim_freq']}H") / 2)
        _letkfoi_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.RUN}.t{self.runtime_config['cyc']:02d}z.letkfoi.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'SNOW_WINDOW_BEGIN': _window_begin,
                'SNOW_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'OPREFIX': f"{self.runtime_config.RUN}.t{self.runtime_config.cyc:02d}z.",
                'APREFIX': f"{self.runtime_config.RUN}.t{self.runtime_config.cyc:02d}z.",
                'jedi_yaml': _letkfoi_yaml
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)


    @logit(logger)
    def initialize(self) -> None:
        """Initialize method for snow ensemble analysis
        This method:


        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """

        super().initialize()


    @logit(logger)
    def execute(self) -> None:
        """Run a series of tasks to create snow ensemble analysis
        This method:


        Parameters
        ----------
        self : Analysis
           Instance of the SnowEnsAnalysis object
        """


    @logit(logger)
    def finalize(self) -> None:
        """Performs closing actions of the snow ensemble analysis task
        This method:
        - 

        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """


    @staticmethod
    @logit(logger)
    def get_bkg_dict(config: Dict) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, sfc_data)
        that are needed for global snow DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        config: Dict
            Dictionary of key-value pairs needed in this method
            Should contain the following keys:
            COM_ATMOS_RESTART_PREV
            DATA
            current_cycle
            ntiles

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 sfc_data RESTART files, this will be a lot simpler when using history files
        rst_dir = os.path.join(config.COM_ATMOS_RESTART_PREV)  # for now, option later?
        run_dir = os.path.join(config.DATA, 'bkg')

        # Start accumulating list of background files to copy
        bkglist = []

        # snow DA needs coupler
        basename = f'{to_fv3time(config.current_cycle)}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        # snow DA only needs sfc_data
        for ftype in ['sfc_data']:
            template = f'{to_fv3time(config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
            for itile in range(1, config.ntiles + 1):
                basename = template.format(tilenum=itile)
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': [run_dir],
            'copy': bkglist
        }
        return bkg_dict


    @staticmethod
    @logit(logger)
    def add_increments(config: Dict) -> None:
        """Executes the program "apply_incr.exe" to create analysis "sfc_data" files by adding increments to backgrounds

        Parameters
        ----------
         config: Dict
             Dictionary of key-value pairs needed in this method
             Should contain the following keys:
             HOMEgfs
             COM_ATMOS_RESTART_PREV
             DATA
             current_cycle
             CASE
             OCNRES
             ntiles
             APPLY_INCR_NML_TMPL
             APPLY_INCR_EXE
             APRUN_APPLY_INCR

        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        # need backgrounds to create analysis from increments after LETKF
        logger.info("Copy backgrounds into anl/ directory for creating analysis from increments")
        template = f'{to_fv3time(config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        anllist = []
        for itile in range(1, config.ntiles + 1):
            filename = template.format(tilenum=itile)
            src = os.path.join(config.COM_ATMOS_RESTART_PREV, filename)
            dest = os.path.join(config.DATA, "anl", filename)
            anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

        logger.info("Create namelist for APPLY_INCR_EXE")
        nml_template = config.APPLY_INCR_NML_TMPL
        nml_data = Jinja(nml_template, config).render
        logger.debug(f"apply_incr_nml:\n{nml_data}")

        nml_file = os.path.join(config.DATA, "apply_incr_nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

        logger.info("Link APPLY_INCR_EXE into DATA/")
        exe_src = config.APPLY_INCR_EXE
        exe_dest = os.path.join(config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # execute APPLY_INCR_EXE to create analysis files
        exe = Executable(config.APRUN_APPLY_INCR)
        exe.add_default_arg(os.path.join(config.DATA, os.path.basename(exe_src)))
        logger.info(f"Executing {exe}")
        try:
            exe()
        except OSError:
            raise OSError(f"Failed to execute {exe}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exe}")
