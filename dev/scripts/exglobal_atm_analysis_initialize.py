#!/usr/bin/env -S python3 -m trace -t --ignore-module=__init__ --ignore-dir="${PYTHON_EXCLUDE_TRACE_PATHS}
# exglobal_atm_analysis_initialize.py
# This script creates an AtmAnalysis class
# and runs the initialize method
# which creates and stages the runtime directory
# and creates the YAML configuration
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

    # Initialize JEDI variational analysis
    AtmAnl.initialize()
