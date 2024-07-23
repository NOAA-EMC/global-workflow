#!/usr/bin/env python3
# exglobal_atmens_analysis_fv3_increment.py
# This script creates an AtmEnsAnalysis object
# and runs the initialize, execute, and finalize methods
# which convert the JEDI increment into an FV3 increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.atmens_analysis_fv3inc import AtmEnsAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atmens analysis task
    AtmEnsAnlFV3Inc = AtmEnsAnalysis(config)
    AtmEnsAnlFV3Inc.initialize()
    AtmEnsAnlFV3Inc.execute(config.APRUN_ATMENSANLFV3INC)
    AtmEnsAnlFV3Inc.finalize()
