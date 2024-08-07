#!/usr/bin/env python3
# exgdas_enkf_snow_recenter.py
# This script creates an SnowEnsAnalysis class
# and will recenter the ensemble mean to the
# deterministic analysis and provide increments
# to create an ensemble of snow analyses
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
    anl.genWeights()
    anl.genMask()
    anl.regridDetBkg()
    anl.regridDetInc()
    anl.recenterEns()
    anl.addEnsIncrements()
    anl.finalize()
