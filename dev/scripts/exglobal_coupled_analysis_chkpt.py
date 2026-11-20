#!/usr/bin/env python3
# exglobal_coupled_analysis_chkpt.py
# This script creates a CoupledAnalysis object
# and runs the checkpoint methods which insert
# the seaice analysis into the CICE6 restart and
# create a soca MOM6 IAU increment
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.coupled_analysis import CoupledAnalysis

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the coupled analysis task
    CoupledAnl = CoupledAnalysis(config)

    # Prepare the SOCA increment for MOM6 IAU and CICE6 restart
    CoupledAnl.execute('soca_incpostproc')

    # Compute the observation space statistics
    try:
        CoupledAnl.execute('soca_diag_stats')
    except Exception as e:
        logger.warning(f"Execution of 'soca_diag_stats' application failed: {e}")
