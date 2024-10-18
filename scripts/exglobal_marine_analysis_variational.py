#!/usr/bin/env python3
# exglobal_marine_analysis_variational.py
# This script creates an MarineAnalysis object
# and runs the execute method
# which executes the global marine variational analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.marine_analysis import MarineAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Create a MarineAnalysis object
    MarineAnl = MarineAnalysis(config)

    # Run the variational application
    MarineAnl.variational()
