#!/usr/bin/env python3
# exglobal_snow_analysis.py
# This script creates an SnowAnalysis class
# and runs the initialize, execute and finalize methods
# for a global Snow Depth analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.snow_analysis import SnowAnalysis

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the snow analysis task
    SnowAnl = SnowAnalysis(config)

    # Initialize JEDI 2DVar snow analysis
    SnowAnl.initialize()

    # Process IMS snow cover (if applicable)
    if SnowAnl.task_config.cyc == 0:
        SnowAnl.prepare_IMS()

    # Execute JEDI snow analysis
    SnowAnl.execute('snowanlvar')

    # Add increments
    SnowAnl.add_increments()

    # Finalize JEDI snow analysis
    SnowAnl.finalize()
