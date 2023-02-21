#!/usr/bin/env python3
# exglobal_land_analysis_initialize.py
# This script creates an LandAnalysis class
# and runs the initialize method
# which create and stage the runtime directory
# and create the YAML configuration
# for a global land letkfoi analysis
import os

from pygw.logger import Logger
from pygw.configuration import cast_strdict_as_dtypedict
from pygfs.task.land_analysis import LandAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the land analysis task
    LandAnl = LandAnalysis(config)
    LandAnl.initialize()
