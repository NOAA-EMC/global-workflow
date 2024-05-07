#!/usr/bin/env python3
# exgdas_global_marine_analysis_ecen.py
# This script creates an MarineLETKF class
# and runs the initialize, run, and finalize  methods
# which currently are stubs
import os

from wxflow import Logger, cast_strdict_as_dtypedict
# TODO (AFE): change to from pygfs.task.marine_recenter import MarineLETKF
from soca.marine_letkf import MarineLETKF

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the aerosol analysis task
    MarineLetkf = MarineLETKF(config)
    MarineLetkf.initialize()
    MarineLetkf.run()
    MarineLetkf.finalize()
