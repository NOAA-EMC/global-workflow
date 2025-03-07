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

    # Instantiate the atm analysis task
    AnlStats = AnalysisStats(config)

    # Create list based on DA components
    AnlStats.task_config['STAT_ANALYSES'] = []
    if AnlStats.task_config.DO_AERO_ANL:
        AnlStats.task_config['STAT_ANALYSES'].append('aero')
    if AnlStats.task_config.DO_JEDISNOWDA:
        AnlStats.task_config['STAT_ANALYSES'].append('snow')
    if AnlStats.task_config.DO_JEDIATMVAR:
        AnlStats.task_config['STAT_ANALYSES'].append('atmos')

    # Initialize JEDI variational analysis
    AnlStats.initialize()
    for anl in AnlStats.task_config.STAT_ANALYSES:
        AnlStats.execute(anl)
        AnlStats.finalize(anl)
