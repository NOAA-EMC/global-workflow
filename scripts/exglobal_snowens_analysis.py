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
    snow_ens_anl = SnowEnsAnalysis(config)

    # Initialize JEDI 2DVar snow analysis
    snow_ens_anl.initialize()

    # Calculate ensemble mean
    snow_ens_anl.execute('esnowanlensmean')

    # stage ensemble mean backgrounds

    # Process IMS snow cover (if applicable)
    if snow_ens_anl.task_config.cyc == 0:
        snow_ens_anl.prepare_IMS()

    # Execute JEDI snow analysis
    snow_ens_anl.execute('snowanlvar')

    # Add increments
    snow_ens_anl.add_increments()

    # Finalize JEDI snow analysis
    snow_ens_anl.finalize()
