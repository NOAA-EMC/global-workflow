#!/usr/bin/env python3
# exgdas_global_marine_analysis_ecen.py
# This script creates an MarineRecenter class
# and runs the initialize, run, and finalize  methods
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.marine_recenter import MarineRecenter

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    #
    MarineRecen = MarineRecenter(config)
    MarineRecen.initialize()
    MarineRecen.execute('gridgen')
    MarineRecen.execute('ens_handler')
    MarineRecen.finalize()
