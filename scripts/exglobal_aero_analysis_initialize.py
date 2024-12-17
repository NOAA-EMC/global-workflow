#!/usr/bin/env python3
# exglobal_aero_analysis_initialize.py
# This script creates an AerosolAnalysis class
# and runs the initialize method
# which create and stage the runtime directory
# and create the YAML configuration
# for a global aerosol variational analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.aero_analysis import AerosolAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the aerosol analysis task
    AeroAnl = AerosolAnalysis(config)

    # Initialize JEDI aerosol variational analysis
    AeroAnl.initialize()
