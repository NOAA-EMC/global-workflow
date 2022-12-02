#!/usr/bin/env python3
################################################################################
#  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_global_aero_analysis_finalize.py
# Script description:  Finalizes runtime directory for global aerosol analysis
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2022-8-29
#
# Abstract: This script cleans up the runtime directory and moves/archives
#           necessary output files ffrom FV3-JEDI executable(s)
#           produced from a UFS Global Aerosol Analysis.
#
# $Id$
#
# Attributes:
#   Language: Python3
#
################################################################################

import gfs.task.aero_analysis
import logging
import os

# set up logger
logging.basicConfig(format='%(asctime)s:%(levelname)s:%(message)s', level=logging.INFO, datefmt='%Y-%m-%d %H:%M:%S')

if __name__ == '__main__':

    AeroAnl = gfs.task.aero_analysis.AerosolAnalysis(os.environ)
    AeroAnl.finalize()
