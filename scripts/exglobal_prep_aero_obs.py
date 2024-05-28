#!/usr/bin/env python3
# exglobal_prep_aero_obs.py
# This script collect available viirs
# obs files, combine and preprocess
# them.
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.aero_prepobs import AerosolObsPrep

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the aerosol prep obs tasks
    AeroObs = AerosolObsPrep(config)
    AeroObs.initialize()
    AeroObs.runConverter()
    AeroObs.finalize()
