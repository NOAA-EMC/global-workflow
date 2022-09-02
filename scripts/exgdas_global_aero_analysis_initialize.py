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


class GlobalAerosolAnalysis:
    """
    Class for global aerosol analysis object
    """
    def __init__(self, config):
        # for now config is assumed to be os.environ
        self.fv3jedi_fix = config['FV3JEDI_FIX']

    def stage_fix(self, datadir):
        """
        Method to stage fix files for FV3-JEDI
        """
        # Note to @aerorahul, I hope to replace this with API calls to the 'r2d2' replacement
        fix_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         f'akbk{self.nlevs}.nc4'): os.path.join(datadir,
                                                                'fv3jedi',
                                                                'akbk.nc4'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         'fmsmpp.nml'): os.path.join(datadir,
                                                     'fv3jedi',
                                                     'fmsmpp.nml'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         'field_table_gfdl'): os.path.join(datadir,
                                                           'fv3jedi',
                                                           'field_table'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fieldmetadata',
                         'gfs-aerosol.yaml'): os.path.join(datadir,
                                                           'fv3jedi',
                                                           'field_table'),
            }
        logging.info('Staging fix files...')
        for src, dest in fix_file_dict.items():
            logging.info(f'Copying {src} to {dest}')
            if not os.path.exists(os.path.dirname(dest)):
                os.makedirs(os.path.dirname(dest))
            if os.path.exists(dest):
                os.remove(dest)
            shutil.copyfile(src, dest)
        logging.info('Completed staging fix files...')


    def stage_crtm_coeff(self, datadir):
        """
        Stage CRTM coefficent files needed
        to run CRTM AOD operator
        """
        coeff_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'AerosolCoeff.bin'): os.path.join(datadir,
                                                           'crtm',
                                                           'AerosolCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'CloudCoeff.bin'): os.path.join(datadir,
                                                           'crtm',
                                                           'CloudCoeff.bin'),
            # Note: Ideally we would only copy files for platforms used
            # but since it is just 2 VIIRS platforms, just copy them both regardless
            # We will fix this when there is a solution for ATM mode
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_npp.SpcCoeff.bin'): os.path.join(datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_npp.TauCoeff.bin'): os.path.join(datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.TauCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_j01.SpcCoeff.bin'): os.path.join(datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_j01.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_j01.TauCoeff.bin'): os.path.join(datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_j01.TauCoeff.bin'),
            }

        logging.info('Staging CRTM coefficients...')
        for src, dest in coeff_file_dict.items():
            logging.info(f'Copying {src} to {dest}')
            if not os.path.exists(os.path.dirname(dest)):
                os.makedirs(os.path.dirname(dest))
            if os.path.exists(dest):
                os.remove(dest)
            shutil.copyfile(src, dest)
        logging.info('Completed staging CRTM coefficients...')


    def stage_berror(self, datadir):
        """
        Stage background error files needed
        """
        # NOTE for the 'plumbing', this will not be done yet, just using identity B
        logging.info('Staging background error files...')
        logging.info('Completed staging background error files...')


def global_aero_analysis_init():
    """
    Top-level function to initialize a runtime directory
    to perform a global aerosol analysis
    """

    # get configuration based on environment variables
    #config = ufsda.misc_utils.get_env_config(component='atm')
    # NOTE for now just going to grab things from os.environ

    # create analysis object
    #aeroanl = GlobalAerosolAnalysis(config)
    aeroanl = GlobalAerosolAnalysis(os.environ)

    # stage FV3-JEDI fix files
    aeroanl.stage_fix(os.environ['DATA'])

    # stage AOD CRTM coefficients
    aeroanl.stage_crtm_coeff(os.environ['DATA'])

    # stage BUMP background error files
    aeroanl.stage_berror(os.environ['DATA'])

    # stage observations

    # stage model backgrounds


if __name__ == '__main__':

    global_aero_analysis_init()
