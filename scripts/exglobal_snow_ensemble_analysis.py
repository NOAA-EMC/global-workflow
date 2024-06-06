#!/usr/bin/env python3
# exglobal_snow_ensemble_analysis.py
# This script creates an SnowEnsAnalysis class
# and runs the initialize, execute and finalize methods
# for a global Snow Depth ensemble analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.snowens_analysis import SnowEnsAnalysis

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the snow ensemble analysis task
    anl = SnowEnsAnalysis(config)
    anl.initialize()
    anl.regridDetBkg()
    anl.regridDetInc()
    anl.recenterEns()
    anl.addIncrements()
    anl.finalize()
