#!/usr/bin/env python3
# exgdas_global_atmens_analysis_run.py
# This script creates an AtmEnsAnalysis object
# and runs the execute method
# which executes the global atm ensemble analysis
import os

from pygw.logger import Logger
from pygw.configuration import cast_strdict_as_dtypedict
from pygfs.task.atmens_analysis import AtmEnsAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atmens analysis task
    AtmEnsAnl = AtmEnsAnalysis(config)
    AtmEnsAnl.execute()
