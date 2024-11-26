#!/usr/bin/env python3
# exglobal_marine_bmat_run.py
# This script creates an marineBmat object
# and runs the execute method
# which executes all the steps necessary to create the global marine B-matrix
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.marine_bmat import MarineBMat

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Create an instance of the MarineBMat task
    marineBMat = MarineBMat(config)
    marineBMat.initialize()
    marineBMat.execute()
    marineBMat.finalize()
