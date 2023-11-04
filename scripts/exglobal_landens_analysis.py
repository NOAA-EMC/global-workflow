#!/usr/bin/env python3
# exglobal_landens_analysis.py
# This script creates an LandEnsAnalysis object
# and runs the initialize, execute and finalize methods
# which executes the global land enkf ensemble analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.landens_analysis import LandEnsAnalysis

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the landens analysis task
    LandEnsAnl = LandEnsAnalysis(config)
    LandEnsAnl.initialize()
    LandEnsAnl.execute()
    LandEnsAnl.finalize()
