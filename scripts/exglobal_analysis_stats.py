#!/usr/bin/env python3
# exglobal_analysis_stats.py
# This script will run the OOPS/JEDI code to

import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.stat_analysis import StatAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Call StatAnalysis
    anl = StatAnalysis(config)
    anl.initialize()
