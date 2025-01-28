#!/usr/bin/env python3

# exglobal_stat_analysis.py
# This script creates a StatAnalysis class
# and runs the initialize, execute, and finalize
# methods which create and stage the runtime directory
# and create the YAML configuration
# for a global stat analysis
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.stat_analysis import StatAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atm analysis task
    StatAnl = StatAnalysis(config)

    # Create list based on DA components
    StatAnl.task_config['STAT_OBS'] = []
    if StatAnl.task_config.DO_AERO:
        StatAnl.task_config['STAT_OBS'].append('aero')
    if StatAnl.task_config.DO_JEDISNOWDA:
        StatAnl.task_config['STAT_OBS'].append('snow')

    # Initialize JEDI variational analysis
    StatAnl.initialize()
    for ob in StatAnl.task_config.STAT_OBS:
        StatAnl.execute(ob)
        StatAnl.finalize(ob)
