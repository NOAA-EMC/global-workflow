#!/usr/bin/env python3

import os
import gzip
import tarfile
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


class AnalysisStats(Task):
    """
    Class for JEDI-based global analysis stats tasks
    """
    @logit(logger, name="AnalysisStats")
    def __init__(self, config: Dict[str, Any]):
        """
        Constructor global analysis stats task
        This method will construct a global analysis stats task.
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
        This method will initialize a global analysis stats task.
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
        expected_keys = ['aero', 'atmos', 'snow']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

        logger.info(f"Copying files to {self.task_config.DATA}/stats")

        # Extract info from stat config file
        analysis_config_dict = parse_j2yaml(self.task_config.STAT_BASE_CONFIG_YAML, self.task_config)

        # Loop through a copy of ob space list
        for analysis in self.task_config.STAT_ANALYSES[:]:
            logger.info(f"Working on analysis type: {analysis}")

            # Copy stat files to DATA path
            input_tar = os.path.join(analysis_config_dict[analysis]['stat_file_path'],
                                     f"{self.task_config['APREFIX']}{analysis_config_dict[analysis]['stat_file_name']}")
            diag_dir_path = os.path.join(self.task_config.DATA, analysis)

            dest = os.path.join(diag_dir_path, analysis_config_dict[analysis]['stat_file_name'])
            logger.info(f"Copying {input_tar} to {dest} ...")
            tarball_list = [[input_tar, dest]]
            FileHandler({'mkdir': [diag_dir_path], 'copy': tarball_list}).sync()

            # Open tar file
            logger.info(f"Open tarred diagnostic files in {dest}")
            with tarfile.open(dest, "r") as tar:
                # Check if tar file is empty
                if not tar.getnames():
                    logger.warning(f"WARNING. The tar file {dest} is empty. No files to extract.")
                    logger.warning("Moving to next analysis ...")
                    # Remove analysis from STAT_ANALYSES and move to next
                    self.task_config.STAT_ANALYSES.remove(analysis)
                    logger.info(f"current analysis list: {self.task_config.STAT_ANALYSES}")
                    continue
                # Extract all files to the current directory
                tar.extractall(path=diag_dir_path)

            self.task_config.OBSSPACES_LIST = []
            for analysis_dict in analysis_config_dict[analysis]['obs spaces']:
                # Gunzip .nc files
                gz_file = os.path.join(diag_dir_path, (analysis_dict['input file'] + ".gz"))

                # Check if the file exists
                if os.path.exists(gz_file):
                    logger.info(f"Now processing {gz_file}")
                    output_file = os.path.join(diag_dir_path, analysis_dict['input file'])
                    # Open the .gz file
                    with gzip.open(gz_file, 'rb') as f_in:
                        with open(output_file, 'wb') as f_out:
                            f_out.write(f_in.read())
                else:
                    logger.warning(f"WARNING. {gz_file} does not exist to extract.")
                    logger.warning("Moving to next analysis ...")
                    continue  # Skip current analysis and move to next

                self.task_config.OBSSPACES_LIST.append(analysis_dict['name'])

            # initialize JEDI application
            logger.info(f"Initializing JEDI ioda-stats extraction application")
            self.jedi_dict[analysis].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of analysis stats

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[jedi_dict_key].execute()

    @logit(logger)
    def finalize(self, jedi_dict_key: str) -> None:
        """Finalize the analysis statistics job.

        This method will finalize the analysis statistics job using JEDI.
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

        analysis_config_dict = parse_j2yaml(self.task_config.STAT_BASE_CONFIG_YAML, self.task_config)

        for analysis_dict in analysis_config_dict[jedi_dict_key]['obs spaces']:
            statfile = os.path.join(self.task_config.DATA, analysis_dict['output file'])
            outdir = self.task_config['COMOUT_' + jedi_dict_key.upper() + '_ANLMON']

            # Check if the directory exists; if not, create it
            if not os.path.exists(outdir):
                FileHandler({'mkdir': [outdir]}).sync()

            dest = os.path.join(outdir, f"{analysis_dict['output file']}")
            logger.debug(f"copying {statfile} to {dest}")
            stat_copy = {
                'copy': [[statfile, dest]]
            }
            FileHandler(stat_copy).sync()
