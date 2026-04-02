#!/usr/bin/env python3
# exglobal_offline_analysis.py
# This script creates an OfflineAnalysis class
# and runs the initialize, run, and finalize methods
# which will stage files, interpolate the offline analysis,
# produce analysis increments from the previous forecast
# and the regridded offline analysis,
# and copy the new increments to COM
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.offline_analysis import OfflineAnalysis

# Initialize root logger
logger = Logger(
    level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the offline analysis task
    offline_anl = OfflineAnalysis(config)

    # Initialize and stage the runtime directory
    offline_anl.initialize()

    # Interpolate the Gaussian analysis to the background resolution
    offline_anl.interpolate_analysis()

    # Compute the increment between the analysis and background
    offline_anl.calc_increment()

    # Compute the tref increment
    offline_anl.calc_tref_inc()

    # Copy the analysis increment and regridded analysis back to COM
    offline_anl.finalize()
