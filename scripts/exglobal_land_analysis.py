#!/usr/bin/env python3
# exglobal_land_analysis.py
# This script creates an LandAnalysis class
# and runs the initialize, execute and finalize methods
# for a global Land Snow Depth analysis
import os

from wxflow.logger import Logger
from wxflow.configuration import cast_strdict_as_dtypedict
from pygfs.task.land_analysis import LandAnalysis

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the land analysis task
    anl = LandAnalysis(config)
    anl.initialize()
    anl.execute()
    anl.finalize()
