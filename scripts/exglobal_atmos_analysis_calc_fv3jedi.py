#!/usr/bin/env python3
# exglobal_atm_calc_analysis.py
# This script creates an AnalysisCalc object
# and runs the execute method which executes
# the diagnostic global analysis calculation
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.analcalc import AnalysisCalc

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the AnalysisCalc task
    AnalCalc = AnalysisCalc(config, 'fv3jedi_analcalc')

    # Initialize
    AnalCalc.initialize_jedi()
    AnalCalc.initialize()

    # Execute JEDI application
    AnalCalc.execute(config.APRUN_ANALCALC_FV3JEDI)

    # Finalize
    AnalCalc.finalize()
