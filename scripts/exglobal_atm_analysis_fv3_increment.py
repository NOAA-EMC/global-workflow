#!/usr/bin/env python3
# exglobal_atm_analysis_fv3_increment.py
# This script creates an AtmAnalysisFV3Inc object
# and runs the initialize, execute, and finalize methods
# which convert the JEDI increment into an FV3-readable
# increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.atm_analysis_fv3inc import AtmAnalysisFV3Inc

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atm analysis fv3inc task
    AtmAnlFV3Inc = AtmAnalysisFV3inc(config)
    AtmAnlFV3Inc.initialize()
    AtmAnlFV3Inc.execute(config.APRUN_ATMANLFV3INC)
    AtmAnlFV3Inc.finalize()
