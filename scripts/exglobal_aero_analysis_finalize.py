#!/usr/bin/env python3
# exgdas_global_aero_analysis_finalize.py
# This script creates an AerosolAnalysis class
# and runs the configure and finalize methods
# which perform post-processing and clean up activities
# for a global aerosol variational analysis
import pygfs.task.aero_analysis
import logging
import os

# set up logger
logging.basicConfig(format='%(asctime)s:%(levelname)s:%(message)s', level=logging.INFO, datefmt='%Y-%m-%d %H:%M:%S')

if __name__ == '__main__':

    AeroAnl = pygfs.task.aero_analysis.AerosolAnalysis(dict(os.environ))
    AeroAnl.configure()
    AeroAnl.finalize()
