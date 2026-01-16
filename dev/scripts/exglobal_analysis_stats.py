#!/usr/bin/env python3

# exglobal_analysis_stats.py
# This script creates a AnalysisStats class
# and runs the initialize, execute, and finalize
# methods which create and stage the runtime directory
# and create the YAML configuration
# to produce summary statistics from the desired analyses
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.analysis_stats import AnalysisStats

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Create list based on DA components
    config.STAT_ANALYSES = []
    if config.DO_AERO_ANL:
        config.STAT_ANALYSES.append('aero')
    if config.DO_JEDISNOWDA:
        config.STAT_ANALYSES.append('snow')
    if config.DO_JEDIATMVAR:
        config.STAT_ANALYSES.append('atmos')
    else:
        config.STAT_ANALYSES.append('atmos_gsi')

    # Instantiate the analysis stats task
    AnlStats = AnalysisStats(config)

    # Initialize JEDI variational analysis
    if not config.DO_JEDIATMVAR:
        AnlStats.convert_gsi_diags()
    AnlStats.initialize()
    for anl in config.STAT_ANALYSES:
        AnlStats.execute(anl)
        AnlStats.finalize(anl)
