#!/usr/bin/env python3
# exglobal_atmens_analysis_initialize.py
# This script creates an AtmEnsAnalysis class
# and runs the initialize method
# which creates and stages the runtime directory
# and creates the YAML configuration
# for a global atm local ensemble analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.atmens_analysis import AtmEnsAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atmens analysis task
    AtmEnsAnl = AtmEnsAnalysis(config)

    # Initialize JEDI ensemble DA analysis
    AtmEnsAnl.initialize()
