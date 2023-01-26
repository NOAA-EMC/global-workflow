#!/usr/bin/env python3
# exgdas_global_aero_analysis_initialize.py
# This script creates an AerosolAnalysis class
# and runs the configure and initialize methods
# which create and stage the runtime directory
# and create the YAML configuration
# for a global aerosol variational analysis
import pygfs.task.aero_analysis
import logging
import os

# set up logger
logging.basicConfig(format='%(asctime)s:%(levelname)s:%(message)s', level=logging.INFO, datefmt='%Y-%m-%d %H:%M:%S')


if __name__ == '__main__':

    AeroAnl = pygfs.task.aero_analysis.AerosolAnalysis(dict(os.environ))
    AeroAnl.configure()
    AeroAnl.initialize()
