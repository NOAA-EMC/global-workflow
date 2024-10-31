#!/usr/bin/env python3
# exgdas_global_marine_analysis_letkf.py
# This script creates an MarineLETKF class
# and runs the initialize, run, and finalize methods
# which currently are stubs
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.marine_letkf import MarineLETKF

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the marine letkf task
    MarineLetkf = MarineLETKF(config)
    MarineLetkf.initialize()
    MarineLetkf.run()
    MarineLetkf.finalize()
