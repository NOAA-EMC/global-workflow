#!/usr/bin/env python3
# exglobal_atmens_analysis_finalize.py
# This script creates an AtmEnsAnalysis class
# and runs the finalize method
# which perform post-processing and clean up activities
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

    # Finalize ensemble DA analysis
    AtmEnsAnl.finalize()
