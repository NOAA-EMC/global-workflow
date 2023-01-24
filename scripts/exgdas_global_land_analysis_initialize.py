#!/usr/bin/env python3
import pygfs.task.land_analysis
import logging
import os

# set up logger
logging.basicConfig(format='%(asctime)s:%(levelname)s:%(message)s', level=logging.INFO, datefmt='%Y-%m-%d %H:%M:%S')


if __name__ == '__main__':

    LandAnl = pygfs.task.land_analysis.LandsolAnalysis(os.environ)
    LandAnl.configure()
    LandAnl.initialize()
