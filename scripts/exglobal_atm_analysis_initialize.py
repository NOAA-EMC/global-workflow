#!/usr/bin/env python3
# exglobal_atm_analysis_initialize.py
# This script creates an AtmAnalysis class
# and runs the initialize method
# which create and stage the runtime directory
# and create the YAML configuration
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
    AtmAnl = AtmAnalysis(config, 'atmanlvar')

    # Initialize JEDI variational analysis
    AtmAnl.initialize_jedi()
    AtmAnl.initialize_analysis()
