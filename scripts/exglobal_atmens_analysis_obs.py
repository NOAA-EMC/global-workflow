#!/usr/bin/env python3
# exglobal_atmens_analysis_obs.py
# This script creates an AtmEnsAnalysis object
# and runs the execute method of its Jedi object attribute
# which executes the global atm local ensemble analysis in observer mode
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

    # Initialize and execute JEDI ensembler DA analysis in observer mode
    AtmEnsAnl.jedi['atmensanlobs'].execute(config.APRUN_ATMENSANLOBS)
