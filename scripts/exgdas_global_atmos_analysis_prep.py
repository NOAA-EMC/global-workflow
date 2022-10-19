#!/usr/bin/env python3
################################################################################
#  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_global_atmos_analysis_prep.py
# Script description:  Stages files and generates YAML for Global Atmosphere Analysis
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2021-12-21
#
# Abstract: This script stages necessary input files and produces YAML
#           configuration input file for FV3-JEDI executable(s) needed
#           to produce a UFS Global Atmospheric Analysis.
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

# use R2D2 to stage obs and bias correction coefficient files
ufsda.stage.atm_obs(config)
ufsda.stage.bias_obs(config)
