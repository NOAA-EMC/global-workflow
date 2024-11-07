#!/usr/bin/env python3
# exglobal_atm_analysis_fv3_increment.py
# This script creates an AtmAnalysis object
# and runs the initialize_fv3inc and execute methods
# which convert the JEDI increment into an FV3 increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.atm_analysis import AtmAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atm analysis object
    AtmAnl = AtmAnalysis(config, 'atmanlfv3inc')

    # Initialize and execute FV3 increment converter
    AtmAnl.initialize_jedi()
    AtmAnl.execute(config.APRUN_ATMANLFV3INC)
