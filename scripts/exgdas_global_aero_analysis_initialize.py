#!/usr/bin/env python3
################################################################################
#  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_global_aero_analysis_initialize.py
# Script description:  Initializes runtime directory for global aerosol analysis
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2022-09-06
#
# Abstract: This script sets up the runtime directory and stages
#           necessary input files for FV3-JEDI executable(s) needed
#           to produce a UFS Global Aerosol Analysis.
#
# $Id$
#
# Attributes:
#   Language: Python3
#
################################################################################

# import os and sys to add ush to path
import logging
import os
import shutil
import sys

# set up logger
logging.basicConfig(format='%(asctime)s:%(levelname)s:%(message)s', level=logging.INFO, datefmt='%Y-%m-%d %H:%M:%S')

# get absolute path of ush/ directory either from env or relative to this file
my_dir = os.path.dirname(__file__)
my_home = os.path.dirname(os.path.dirname(my_dir))
sys.path.append(os.path.join(os.getenv('HOMEgfs', my_home), 'ush'))
logging.info(f"sys.path={sys.path}")

# import UFSDA utilities
import ufsda

# import GFS workflow module
import gfs.task.analysis

if __name__ == '__main__':

    AeroAnl = gfs.task.analysis.AerosolAnalysis(os.environ)
    AeroAnl.initialize()
