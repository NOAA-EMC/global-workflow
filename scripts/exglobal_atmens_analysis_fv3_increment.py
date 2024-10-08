#!/usr/bin/env python3
# exglobal_atmens_analysis_fv3_increment.py
# This script creates an AtmEnsAnalysis object
# and runs the initialize_fv3inc and execute methods
# which convert the JEDI increment into an FV3 increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.atmens_analysis import AtmEnsAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atmens analysis object
    AtmEnsAnl = AtmEnsAnalysis(config, 'atmensanlfv3inc')

    # Initialize and execute JEDI FV3 increment converter
    AtmEnsAnl.initialize_jedi()
    AtmEnsAnl.execute(config.APRUN_ATMENSANLFV3INC)
