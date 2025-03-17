#!/usr/bin/env python
# fetch-fix-data.py
# wei.huang@noaa.gov
# 2025-02-26
# script to download a subset of FIX data to local machines.
import os
import time
import sys
import getopt
import argparse
import subprocess
from pathlib import Path
import logging

# Create and configure logger
logging.basicConfig(filename="fetch-fix-data.log",
                    format='%(asctime)s %(message)s',
                    filemode='w')

# Creating an object
logger = logging.getLogger()

# Setting the threshold of logger to DEBUG
logger.setLevel(logging.DEBUG)

# ------------------------------------------------------------------------------


class FetchFIXdata():
    """Fetch a subset of FIX data from NOAA s3 bucket.
    """

    def __init__(self, atmgridarray=['C48'], ocngridarray=['500'],
                 fix_bucket=None, fix_ver=None, localdir=None, verbose=False):
        """Constructor for FetchFIXdata
        The constructor is responsible for collecting necessary parameters.

        Parameters
        ----------
        atmgrdiarray: list
            A list of ATM grids
        ocngrdiarray: list
            A list of OCN grids
        fix_bucket: str
            NOAA s3 bucket of Global-Workflow full FIX data
        fix_ver: str
            FIX version file
        localdir: str
            Local dir to store the subset of FIX data.

        Returns
        -------
        None
        """
        # self.aws_fix_bucket = f's3://noaa-nws-global-pds/fix'
        self.aws_fix_bucket = fix_bucket
        self.aws_cp = f'aws --no-sign-request s3 cp'
        self.aws_sync = f'aws --no-sign-request s3 sync'

        self.atmgridarray = atmgridarray
        self.ocngridarray = ocngridarray
        self.localdir = localdir
        self.fix_ver = fix_ver
        self.verbose = verbose

        logger.info(f'localdir: {localdir}')
        logger.info(f'fix_ver: {fix_ver}')
        logger.info(f'fix_buck: {fix_bucket}')
        logger.info(f'verbose: {verbose}')

        if (os.path.isdir(localdir)):
            logger.info(f'Prepare to download FIX data for {atmgrid} and {ocngrid} to {localdir}')
        else:
            logger.error(f'local dir: <{localdir}> does not exist. Stop')
            raise SystemExit

        if (os.path.isfile(fix_ver)):
            logger.info(f'Prepare to read FIX data for {atmgrid} and {ocngrid} to {fix_ver}')
        else:
            logger.error(f'File fix_ver: <{fix_ver}> does not exist. Stop')
            raise SystemExit

        self.s3dict = {}
        self.s3dict['raworog'] = f'raw/orog'

        if (self.localdir.find('fix') < 0):
            self.targetdir = f'{self.localdir}/fix.subset'
        else:
            self.targetdir = self.localdir

        self.get_fix_ver_dict()
        self.create_s3dict()

    # --------------------------------------------------------------------------
    def create_s3dict(self):
        """
        Create a dictionay based on fix_ver file,
        corresponding to FIX data s3 bucket directory.
        returns
        ----------
        None
        """
        for key in self.fix_ver_dict.keys():
            val = self.fix_ver_dict[key]
            s3key, _ = key.split('_ver')
            if (s3key == 'chem'):
                self.s3dict['fimdata_chem'] = f'chem/{val}/fimdata_chem'
                self.s3dict['Emission_data'] = f'chem/{val}/Emission_data'
            elif (s3key == 'datm'):
                self.s3dict['cfsr'] = f'datm/{val}/cfsr'
                self.s3dict['gefs'] = f'datm/{val}/gefs'
                self.s3dict['gfs'] = f'datm/{val}/gfs'
                self.s3dict['mom6'] = f'datm/{val}/mom6'
            else:
                if (s3key in ['orog', 'ugwd']):
                    self.add_atmgrid2s3dict(s3key, val)
                elif (s3key in ['mom6', 'cice']):
                    self.add_ocngrid2s3dict(s3key, val)
                elif (s3key == 'cpl'):
                    self.add_cpl2s3dict(s3key, val)
                else:
                    self.s3dict[s3key] = f'{s3key}/{val}'

        if (self.verbose):
            self.printinfo()

    # --------------------------------------------------------------------------
    def add_atmgrid2s3dict(self, key, val):
        """
        Add ATM grid data to dict.
        returns
        ----------
        None
        """
        for atmgrid in self.atmgridarray:
            newkey = f'{key}_{atmgrid}'
            self.s3dict[newkey] = f'{key}/{val}/{atmgrid}'

    # -------------------------------------------------------------------------
    def add_ocngrid2s3dict(self, key, val):
        """
        Add OCN grid data to dict.
        returns
        ----------
        None
        """
        for ocngrid in self.ocngridarray:
            newkey = f'{key}_{atmgrid}'
            self.s3dict[newkey] = f'{key}/{val}/{ocngrid}'

    # -------------------------------------------------------------------------
    def add_cpl2s3dict(self, key, val):
        """
        Add CPL (ATM and OCN complar) grid data to dict.
        returns
        ----------
        None
        """
        for atmgrid in self.atmgridarray:
            for ocngrid in self.ocngridarray:
                newkey = f'{key}_a{atmgrid}o{ocngrid}'
                self.s3dict[newkey] = f'{key}/{val}/a{atmgrid}o{ocngrid}'

    # -------------------------------------------------------------------------
    def printinfo(self):
        """Print dict info (data to download)
        """
        print(f'Preparing to fetch')
        print(f'ATM grid: {self.atmgridarray}')
        print(f'ONC grid: {self.ocngridarray}')
        print(f'From: {self.aws_fix_bucket}')
        print(f'To: {self.targetdir}')
        for key in self.s3dict.keys():
            val = self.s3dict[key]
            print(f'{key}: {val}')

    # -------------------------------------------------------------------------
    def fetchdata(self):
        """Fetch data defined in s3bucket.
        """
        if (self.verbose):
            logger.info(f'Create local fix dir: {self.targetdir}')

        path = Path(self.targetdir)
        path.mkdir(parents=True, exist_ok=True)

        self.fetch_ugwp_limb_tau()

        for key in self.s3dict.keys():
            self.download_dir(self.s3dict[key])

    # --------------------------------------------------------------------------
    def download_dir(self, dir):
        """download a directory
        """
        remotedir = f'{self.aws_fix_bucket}/{dir}'
        localdir = f'{self.targetdir}/{dir}'
        cmd = f'{self.aws_sync} {remotedir} {localdir}'

        # returned_value = os.system(cmd)  # returns the exit code in unix
        # if (self.verbose):
        #     logger.info(f'returned value: {returned_value}')

        if (os.path.isdir(localdir)):
            logger.info(f'{localdir} already exist. skip')
        else:
            parentdir, dirname = os.path.split(localdir)
            if (self.verbose):
                logger.info(f'Create local {parentdir} dir:')
            path = Path(parentdir)
            path.mkdir(parents=True, exist_ok=True)
            if (self.verbose):
                logger.info(cmd)
            logger.info(f'Downloading {localdir}')
            returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix
            if (self.verbose):
                logger.info(f'returned value: {returned_value}')

    # --------------------------------------------------------------------------
    def fetch_ugwp_limb_tau(self):
        """download ugwp_limb_tau.nc
        """
        ugwd_ver = self.fix_ver_dict['ugwd_ver']
        ugwp_limb_tau_remotepath = f'{self.aws_fix_bucket}/ugwd/{ugwd_ver}/ugwp_limb_tau.nc'
        ugwp_limb_tau_localdir = f'{self.targetdir}/ugwd/{ugwd_ver}'
        filename = f'{ugwp_limb_tau_localdir}/ugwp_limb_tau.nc'
        path = Path(ugwp_limb_tau_localdir)
        path.mkdir(parents=True, exist_ok=True)
        cmd = f'{self.aws_cp} {ugwp_limb_tau_remotepath} {filename}'

        # returned_value = os.system(cmd)  # returns the exit code in unix
        # if (self.verbose):
        #     logger.info(f'returned value: {returned_value}')

        if (os.path.isfile(filename)):
            logger.info(f'{filename} already exist. skip')
        else:
            if (self.verbose):
                logger.info(cmd)
            logger.info(f'Downloading {filename}')
            returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix
            if (self.verbose):
                logger.info(f'returned value: {returned_value}')

    # --------------------------------------------------------------------------
    def get_fix_ver_dict(self):
        """Get fix ver as dictionay from FIX ver file.
        """
        self.fix_ver_dict = {}
        with open(self.fix_ver, "r") as file:
            for line in file.readlines():
                if (line.find('export ') >= 0):
                    key, value = line.replace('export ', '', 1).split('=')
                    # skip gdas data, for DA projects, one should keep gdas part.
                    if (key.find('gdas_') >= 0):
                        continue
                    # skip nest data
                    if (key.find('nest') > 0):
                        continue
                    self.fix_ver_dict[key] = value.strip()


# ------------------------------------------------------------------------------
def main() -> None:

    # define available ATM and OCN grids.
    ATMGRIDLIST = ['C48', 'C96', 'C192', 'C384', 'C768', 'C1152']
    OCNGRIDLIST = ['500', '100', '050', '025']

    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="increase output verbosity")
    parser.add_argument("-d", "--localdir", type=str, required=True,
                        help="local directory to store FIX data subset")
    parser.add_argument("-f", "--fix_ver", type=str, required=True,
                        default="unknown",
                        help="fix.ver file from Global-Workflow versions directory")
    parser.add_argument("-b", "--fix_bucket", type=str, required=False,
                        default="s3://noaa-nws-global-pds/fix",
                        help="Optional S3 Bucket directory of FIX data, default <s3://noaa-nws-global-pds/fix>")
    parser.add_argument("-a", "--atmgrid", type=str, required=False,
                        default="C48",
                        help="ATM grid, like: C48,C96,C192,C384,C768,C1152, default: C48")
    parser.add_argument("-o", "--ocngrid", type=str, required=False,
                        default="100",
                        help="OCN grid, like: 500,100,050,025, default: 100")
    args = parser.parse_args()

    if args.verbose:
        print(f"the atmgrid is {args.atmgrid}")
        print(f"the ocngrid is {args.ocngrid}")
        print(f"the localdir is {args.localdir}")
        print(f"the fix_file is {args.fix_ver}")
        print(f"the s3 bucket is {args.fix_bucket}")

    atmgrid = args.atmgrid
    if (atmgrid.find(',') > 0):
        atmgridarray = atmgrid.split(',')
    else:
        atmgridarray = [atmgrid]

    for grid in atmgridarray:
        if (grid not in ATMGRIDLIST):
            logger.error(f'atmgrid: {grid}')
            logger.error(f'is not in supported grids: {ATMGRIDLIST}')
            raise SystemExit

    ocngrid = args.ocngrid
    if (ocngrid.find(',') > 0):
        ocngridarray = ocngrid.split(',')
    else:
        ocngridarray = [ocngrid]

    for grid in ocngridarray:
        if (grid not in OCNGRIDLIST):
            logger.error(f'ocngrid: {grid}')
            logger.error(f'is not in supported grids: {OCNGRIDLIST}')
            raise SystemExit

    # ----------------------------------------------------------------------
    ffd = FetchFIXdata(atmgridarray=atmgridarray, ocngridarray=ocngridarray,
                       fix_ver=args.fix_ver, fix_bucket=args.fix_bucket,
                       localdir=args.localdir, verbose=args.verbose)

    ffd.fetchdata()


# ------------------------------------------------------------------------------
if __name__ == '__main__':
    main()
