#!/usr/bin/env python3
# exglobal_atmens_analysis_fv3_increment.py
# This script creates an AtmEnsAnalysis object
# and runs the initialize, execute, and finalize methods
# through its parent class, JEDI, which convert the JEDI
# increment into an FV3 increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict

from pygfs.task.jedi import JEDI
from pygfs.task.atmens_analysis import AtmEnsAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atmens analysis object
    AtmEnsAnlFV3Inc = AtmEnsAnalysis(config)

    # Call all methods through the parent class, JEDI
    JEDI.initialize(AtmEnsAnlFV3Inc)
    JEDI.execute(AtmEnsAnlFV3Inc, config.APRUN_ATMENSANLFV3INC)
    JEDI.finalize(AtmEnsAnlFV3Inc)
