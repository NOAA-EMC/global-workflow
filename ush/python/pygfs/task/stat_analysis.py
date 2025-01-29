#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
import yaml
from logging import getLogger
from pprint import pformat
from typing import Optional, Dict, Any

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta,
                    Task,
                    parse_j2yaml,
                    logit)
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class StatAnalysis(Task):
    """
    Class for JEDI-based global stat analysis tasks
    """
    @logit(logger, name="StatAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """
        Constructor global stat analysis task
        This method will construct a global stat analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute objects
        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration
        Returns
        ----------
        None
        """
        super().__init__(config)

        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'STAT_WINDOW_BEGIN': _window_begin,
                'STAT_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z."
            }
        )
        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self) -> None:
        """
        Initialize a global stat analysis
        This method will initialize a global stat analysis.
        This includes:
        - initialize JEDI applications
        - copying stat files
        Parameters
        ----------
        None
        Returns
        ----------
        None
        """
        # Create dictionary of Jedi objects
        # Expected keys are what must be included from the JEDI config file. We can
        # then loop through ob space list from scripts/exglobal_analysis_stats.py
        expected_keys = ['aero', 'snow']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

        logger.info(f"Copying files to {self.task_config.DATA}/stats")

        # Loop through ob space list
        for ANL in self.task_config.STAT_ANALYSES:
            logger.info(f"Working on current observation: {ANL}")

            # Parse JEDI analysis stat jinja file
            obs_dict = parse_j2yaml(self.task_config.JEDI_CONFIG_YAML, self.task_config)

            # Copy stat files to DATA path
            instat_files = os.path.join(obs_dict[ANL]['stat_file_path'], f"{self.task_config['APREFIX']}{obs_dict[ANL]['stat_file_name']}")
            ob_dir_str = f"{self.task_config.DATA}" + f"/{ANL}"
            os.mkdir(ob_dir_str)

            dest = os.path.join(ob_dir_str, obs_dict[ANL]['stat_file_name'])
            logger.info(f"Copying {instat_files} to {dest} ...")
            statlist = [[instat_files, dest]]
            FileHandler({'copy': statlist}).sync()

            # Open tar file
            logger.info(f"Open tarred stat file in {dest}")
            with tarfile.open(dest, "r") as tar:
                # Check if tar file is empty
                if not tar.getnames():
                    logger.warning(f"WARNING. The tar file {dest} is empty. No files to extract.")
                    logger.warning("Moving to next analysis ...")
                    continue  # Skip current analysis and move to next
                # Extract all files to the current directory
                tar.extractall(path=f'{ob_dir_str}')

            # Gunzip .nc files
            logger.info("Gunzip files from tar file")
            gz_files = glob.glob(os.path.join(ob_dir_str, "*.gz"))

            # Check if gunzip files exist
            if not gz_files:
                logger.warning("WARNING. No .gz files to extract.")
                logger.warning("Moving to next analysis ...")
                continue  # Skip current analysis and move to next

            logger.info(f"Gunzip files: {gz_files}")

            for diagfile in gz_files:
                output_file = os.path.join(ob_dir_str, os.path.basename(diagfile)[:-3])
                with gzip.open(diagfile, 'rb') as f_in:
                    with open(output_file, 'wb') as f_out:
                        f_out.write(f_in.read())

            # Get list of .nc4 files
            # obs_space_paths = glob.glob(os.path.join(ob_dir_str, "*.{nc,nc4}")) # THIS SHOULD WORK BUT ISNT, glob patterns introduced in Python 3.9
            nc_paths = glob.glob(os.path.join(ob_dir_str, "*.nc"))
            nc4_paths = glob.glob(os.path.join(ob_dir_str, "*.nc4"))
            obs_space_paths = nc_paths + nc4_paths

            # Temporary. Create condition check here for available jcb algorithms?
            if ANL == 'snow':
                obs_space_paths = glob.glob(os.path.join(ob_dir_str, "diag_ims_snow_*.nc"))

            # This grabs the obspace string from the .nc4 files, however not all are perfect. Need solution.
            self.task_config.OBSPACES_LIST = ['_'.join(os.path.basename(path).split('_')[1:3]) for path in obs_space_paths]

            # initialize JEDI application
            logger.info(f"Initializing JEDI variational DA application")
            logger.info(f"{self.jedi_dict[ANL]}")
            self.jedi_dict[ANL].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of stat analysis

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """
        logger.info(f"In execute. {self.jedi_dict[jedi_dict_key]}")
        self.jedi_dict[jedi_dict_key].execute()

    @logit(logger)
    def finalize(self, jedi_dict_key: str) -> None:
        """Finalize the statistic analysis job.

        This method will finalize the statistic analysis job using JEDI.
        This includes:
        - copying stat files to specified outdir

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        # get list of output diag files
        diags = glob.glob(os.path.join(self.task_config.DATA, '*output_*.nc'))

        for diagfile in diags:
            outfile = os.path.basename(diagfile)
            dest = os.path.join(f'{self.task_config.STAT_OUTDIR}/{jedi_dict_key}/', f'{outfile}')
            logger.debug(f"copying {diagfile} to {dest}")
            diag_copy = {
                'copy': [[diagfile, dest]]
            }
            FileHandler(diag_copy).sync()
