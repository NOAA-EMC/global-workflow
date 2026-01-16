#!/usr/bin/env python3
# exglobal_atmos_analcalc_fv3jedi.py
# This script creates an FV3AnalysisCalc object
# and runs the initialize, execute, and finalize
# methods which executes the analysis calculation
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.fv3_analysis_calc import FV3AnalysisCalc

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the FV3AnalysisCalc task
    fv3_analysis_calc = FV3AnalysisCalc(config)

    # Initialize, execute, finalize
    fv3_analysis_calc.initialize()
    fv3_analysis_calc.execute()
    fv3_analysis_calc.finalize()
