#!/usr/bin/env -S python3 -m trace --trace --ignore-dir=${PYTHON_EXCLUDE_TRACE_PATHS}
# exglobal_atm_analysis_variational.py
# This script creates an AtmAnalysis object
# and runs the execute method which runs the JEDI
# variational analysis application
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

    # Execute JEDI variational analysis
    AtmAnl.execute('atmanlvar')
