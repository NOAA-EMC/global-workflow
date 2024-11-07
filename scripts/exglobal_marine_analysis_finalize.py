#!/usr/bin/env python3
# exglobal_marine_analysis_finalize.py
# This script creates an MarineAnalysis object
# and makes copies of the variational analysis output
# to the COMROOT
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

    # Make a copy of the analysis output to the COMROOT
    MarineAnl.finalize()

    # Compute the observation space statistics
    MarineAnl.obs_space_stats()
