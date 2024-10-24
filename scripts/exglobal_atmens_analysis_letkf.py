#!/usr/bin/env python3
# exglobal_atmens_analysis_letkf.py
# This script creates an AtmEnsAnalysis object
# and runs the execute method of its Jedi object attribute
# which executes the global atm local ensemble analysis
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

    # Initalize JEDI ensemble DA application
    # Note: This is normally done in AtmEnsAnl.initialize(), but that method now
    #       initializes the split observer-solver. This case is just for testing.
    AtmEnsAnl.jedi['atmensanlletkf'].initialize(AtmEnsAnl.task_config)

    # Execute the JEDI ensemble DA analysis
    AtmEnsAnl.jedi['atmensanlletkf'].execute(config.APRUN_ATMENSANLLETKF)
