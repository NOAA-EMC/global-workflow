#!/usr/bin/env python3
# exgdas_enkf_ecen_fv3jedi.py
# This script creates an EnsembleRecenter object
# and runs the initialize, execute, and finalize
# methods which executes the ensemble recentering
# calculation
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.ensemble_recenter import EnsembleRecenter

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the EnsembleRecenter task
    ensemble_recenter = EnsembleRecenter(config)

    # Initialize, execute, finalize
    ensemble_recenter.initialize()
    ensemble_recenter.execute()
    ensemble_recenter.finalize()
