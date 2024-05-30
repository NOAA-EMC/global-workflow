#!/usr/bin/env python3
# exglobal_prep_emissions.py
# This script creates a emissions object
# which perform the pre-processing for aerosol emissions
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs import AerosolEmissions


# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the emissions pre-processing task
    emissions = AerosolEmissions(config)
    emissions.initialize()
    emissions.configure()
    emissions.execute(emissions.task_config.DATA, emissions.task_config.APRUN)
    emissions.finalize()
