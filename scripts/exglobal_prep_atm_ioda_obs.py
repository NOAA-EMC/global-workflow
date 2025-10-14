#!/usr/bin/env python3
# exglobal_prep_atm_ioda_obs.py
# This script either, depending on configuration,
# will process atmospheric observations into IODA format
# or will copy pre-processed observations from ObsForge
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.atm_analysis import AtmAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the atm analysis task
    AtmAnl = AtmAnalysis(config)

    if AtmAnl.task_config.DO_CONVERT_IODA:
        logger.info('converting observations to IODA format')
        AtmAnl.generate_ioda_obs()
    else:
        # just sync files from COMINobsforge
        logger.info('syncing files from COMINobsforge')
        AtmAnl.stage_ioda_obs()
