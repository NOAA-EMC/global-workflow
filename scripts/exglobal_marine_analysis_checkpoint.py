#!/usr/bin/env python3
# exglobal_marine_analysis_checkpoint.py
# This script creates a MarineAnalysis object
# and runs the checkpoint methods which inserts
# the seaice analysis into the CICE6 restart or
# create a soca MOM6 IAU increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.marine_analysis import MarineAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Create a MarineAnalysis object
    MarineAnl = MarineAnalysis(config)

    # Prepare the SOCA increment for MOM6 IAU
    MarineAnl.checkpoint_mom6_iau('socaincr2mom6.yaml')

    # Insert the seaice analysis into the CICE6 restarts in 2 sequential stages
    MarineAnl.checkpoint_cice6('soca_2cice_arctic.yaml')
    MarineAnl.checkpoint_cice6('soca_2cice_antarctic.yaml')
