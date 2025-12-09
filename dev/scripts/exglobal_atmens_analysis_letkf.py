#!/usr/bin/env python3
# exglobal_atmens_analysis_letkf.py
# This script creates an AtmEnsAnalysis object
# and initializes and runs the full JEDI LETKF
# application
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

    # Initalize JEDI full ensemble DA application
    AtmEnsAnl.initialize_letkf()

    # Execute the JEDI ensemble DA analysis
    AtmEnsAnl.execute('atmensanlletkf')
