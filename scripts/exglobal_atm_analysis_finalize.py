#!/usr/bin/env python3
# exglobal_atm_analysis_finalize.py
# This script creates an AtmAnalysis class
# and runs the finalize method
# which perform post-processing and clean up activities
# for a global atm variational analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.atm_analysis import AtmAnalysis


# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atm analysis task
    AtmAnl = AtmAnalysis(config)

    # Finalize JEDI variational analysis
    AtmAnl.finalize()
