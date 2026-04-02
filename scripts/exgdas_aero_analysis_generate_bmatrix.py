#!/usr/bin/env python3
# exgdas_aero_analysis_generate_bmatrix.py
# This script creates an AerosolBMatrix object
# and runs the methods needed
# to stage files, compute the variance, and write to com
# files needed for the variational solver
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.aero_bmatrix import AerosolBMatrix

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Create an instance of the AerosolBMatrix task
    aeroBMat = AerosolBMatrix(config)
    aeroBMat.initialize()
    aeroBMat.execute()
    aeroBMat.finalize()
