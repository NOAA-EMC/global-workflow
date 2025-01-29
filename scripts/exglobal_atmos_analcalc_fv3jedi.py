#!/usr/bin/env python3
# exglobal_atmos_analcalc_fv3jedi.py
# This script creates an AnalysisCalc object
# and runs the initialize, execute, and finalize
# methods which executes the analysis calculation
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.analysis_calc import AnalysisCalc

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the AnalysisCalc task
    AnalCalc = AnalysisCalc(config)

    # Initialize, execute, finalize
    AnalCalc.initialize()
    AnalCalc.execute()
    AnalCalc.finalize()
