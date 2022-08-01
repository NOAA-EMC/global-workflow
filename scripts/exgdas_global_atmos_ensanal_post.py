#!/usr/bin/env python3
################################################################################
#  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_global_atmos_analysis_post.py
# Script description:  Post atmospheric analysis script.
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2021-12-29
#
# Abstract: This script runs after the atmospheric analysis and
#           archives each diagnostic file into the R2D2 local user database.
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
import sys

# set up logger
logging.basicConfig(format='%(asctime)s:%(levelname)s:%(message)s', level=logging.INFO, datefmt='%Y-%m-%d %H:%M:%S')

# get absolute path of ush/ directory either from env or relative to this file
my_dir = os.path.dirname(__file__)
my_home = os.path.dirname(os.path.dirname(my_dir))
gdas_home = os.path.join(os.environ['HOMEgfs'], 'sorc', 'gdas.cd')
sys.path.append(os.path.join(os.getenv('HOMEgfs', my_home), 'ush'))
logging.info(f"sys.path={sys.path}")

# import UFSDA utilities
import ufsda

# get configuration based on environment variables
config = ufsda.misc_utils.get_env_config(component='atm')
config['DIAG_DIR'] = os.path.join(os.environ['COMOUT_ENS'], 'diags')
config['provider'] = 'ncdiag_lgetkf'

# use R2D2 to archive hofx files
ufsda.archive.atm_diags(config)
