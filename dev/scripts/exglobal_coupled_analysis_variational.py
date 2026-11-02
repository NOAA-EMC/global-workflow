#!/usr/bin/env python3
# exglobal_coupled_analysis_variational.py
# This script creates a CoupledAnalysis object
# and runs the execute method which runs the JEDI
# coupled variational analysis application
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.coupled_analysis import CoupledAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the coupled analysis task
    CoupledAnl = CoupledAnalysis(config)

    # Execute JEDI coupled variational analysis
    CoupledAnl.execute('coupledanlvar')
