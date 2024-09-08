#!/usr/bin/env python3
# exglobal_atm_calc_analysis.py
# This script creates an CalcAnalysis object
# and runs the execute method which executes
# the diagnostic global analysis calculation
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.calcanl import CalcAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the CalcAnalysis task
    CalcAnl = CalcAnalysis(config, 'calcanl')

    # Initialize
    CalcAnl.initialize_jedi()
    CalcAnl.initialize()

    # Execute JEDI application
    CalcAnl.execute(config.APRUN_CALCANL)
