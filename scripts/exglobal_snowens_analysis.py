#!/usr/bin/env python3
# exglobal_snowens_analysis.py
# This script creates an SnowEnsAnalysis class,
# which will compute the ensemble mean of the snow forecast,
# run a 2DVar analysis, and provide increments
# to create an ensemble of snow analyses
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.snowens_analysis import SnowEnsAnalysis

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the snow ensemble analysis task
    SnowEnsAnl = SnowEnsAnalysis(config)

    # Initialize JEDI 2DVar snow analysis
    SnowEnsAnl.initialize()

    # Calculate ensemble mean
    SnowEnsAnl.execute('esnowanlensmean')

    # stage ensemble mean backgrounds

    # Process IMS snow cover (if applicable)
    if SnowEnsAnl.task_config.cyc == 0:
        SnowEnsAnl.prepare_IMS()

    # Execute JEDI snow analysis
    SnowEnsAnl.execute('snowanlvar')

    # Add increments
    SnowEnsAnl.add_increments()

    # Finalize JEDI snow analysis
    SnowEnsAnl.finalize()
