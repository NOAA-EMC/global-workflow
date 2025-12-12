#!/usr/bin/env python3
# exglobal_prep_emissions.py
# This script creates a emissions object
# which perform the pre-processing for aerosol emissions
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs import ChemFireEmissions, NEXUSEmissions


# Initialize root logger
logger = Logger(
    level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)
    nxsemis = NEXUSEmissions(config.copy())
    # Instantiate the emissions pre-processing task
    fireemis = ChemFireEmissions(config.copy())
    fireemis.initialize()
    fireemis.configure()
    fireemis.execute()
    fireemis.finalize()

    nxsemis.initialize()
    nxsemis.configure()
    nxsemis.execute()
    nxsemis.finalize()
