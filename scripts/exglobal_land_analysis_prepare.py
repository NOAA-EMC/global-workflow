#!/usr/bin/env python3
# exglobal_land_analysis_prepare.py
# This script creates a LandAnalysis object
# and runs the prepare method
# which perform the pre-processing for IMS data
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

    # Instantiate the land prepare task
    LandAnl = LandAnalysis(config)
    LandAnl.prepare()
