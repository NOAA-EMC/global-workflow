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
                    add_to_datetime, to_fv3time, to_timedelta, to_YMDH,
                    Task,
                    parse_j2yaml, save_as_yaml,
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

        _res = int(self.task_config.CASE[1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npz_anl': self.task_config.LEVS - 1,
                'STAT_WINDOW_BEGIN': _window_begin,
                'STAT_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z."
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config)

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
        logger.info(f"Copying files to {self.task_config.DATA}/stats")

        # Copy stat files to DATA path
        aerostat = os.path.join(self.task_config.COM_CHEM_ANALYSIS, f"{self.task_config['APREFIX']}aerostat")
        dest = os.path.join(self.task_config.DATA, "aerostats")
        statlist = [[aerostat, dest]]
        FileHandler({'copy': statlist}).sync()

        # Open tar file
        logger.info(f"Open tarred stat file in {dest}")
        with tarfile.open(dest, "r") as tar:
            # Extract all files to the current directory
            tar.extractall()

        # Gunzip .nc files
        logger.info("Gunzip files from tar file")
        gz_files = glob.glob(os.path.join(self.task_config.DATA, "*gz"))

        for diagfile in gz_files:
            with gzip.open(diagfile, 'rb') as f_in:
                with open(diagfile[:-3], 'wb') as f_out:
                    f_out.write(f_in.read())

        # Get list of .nc4 files
        obs_space_paths = glob.glob(os.path.join(self.task_config.DATA, "*.nc4"))

        self.task_config.OBSPACES_LIST = ['_'.join(os.path.basename(path).split('_')[1:3]) for path in obs_space_paths]

        # initialize JEDI application
        logger.info(f"Initializing JEDI variational DA application")
        self.jedi_dict['statanl'].initialize(self.task_config)