#!/usr/bin/env python3
# exglobal_coupled_analysis_finalize.py
# This script creates a CoupledAnalysis object
# and runs the finalize method
# which saves the analysis products and
# observation diagnostics to COM
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

    # Finalize the coupled analysis
    CoupledAnl.finalize()
