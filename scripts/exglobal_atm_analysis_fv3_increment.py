#!/usr/bin/env python3
# exglobal_atm_analysis_fv3_increment.py
# This script creates an AtmAnalysis object
# and runs the initialize, execute, and finalize methods
# through its parent class, JEDI, which convert the JEDI
# increment into an FV3-readable increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict

from pygfs.task.jedi import JEDI
from pygfs.task.atm_analysis import AtmAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atm analysis object
    AtmAnlFV3Inc = AtmAnalysis(config)

    # Run all methods through the parent class, JEDI
    JEDI.initialize(AtmAnlFV3Inc)
    JEDI.execute(AtmAnlFV3Inc, config.APRUN_ATMANLFV3INC)
    JEDI.finalize(AtmAnlFV3Inc)
