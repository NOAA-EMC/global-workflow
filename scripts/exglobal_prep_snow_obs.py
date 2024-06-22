#!/usr/bin/env python3
# exglobal_prep_snow_obs.py
# This script creates a SnowAnalysis object
# and runs the prepare_GTS and prepare_IMS method
# which perform the pre-processing for GTS and IMS data
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.snow_analysis import SnowAnalysis


# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the snow prepare task
    SnowAnl = SnowAnalysis(config)
    SnowAnl.prepare_GTS()
    if f"{ SnowAnl.task_config.cyc }" == '18':
        SnowAnl.prepare_IMS()
