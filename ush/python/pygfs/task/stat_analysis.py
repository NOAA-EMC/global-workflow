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

class StatAnalysis(Analysis):
    """
    Class for global stat analysis tasks
    """

    @logit(logger, name="StatAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.task_config['CASE'][1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)
        _letkfoi_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config['cyc']:02d}z.letkfoi.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'jedi_yaml': _letkfoi_yaml
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """
        Initialize global stat analysis.
        """
        super().initialize()

        logger.info(f"Copying files to {self.task_config.DATA}/stats")

        aerostat = os.path.join(self.task_config.COM_CHEM_ANALYSIS, f"{self.task_config['APREFIX']}aerostat")
        dest = os.path.join(self.task_config.DATA, "stats")
        statlist = [aerostat, dest]
        FileHandler({'copy': statlist}).sync()

    