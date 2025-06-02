#!/usr/bin/env python3
# exglobal_aero_analysis_variational.py
# This script creates an AerosolAnalysis object
# and runs the variational method
# which executes the global aerosol variational analysis
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

    # Execute JEDI variational analysis
    AeroAnl.execute('aeroanlvar')
