<<<<<<< HEAD
#!/usr/bin/env python
# cfetch-fix-data.py
=======
#!/usr/bin/env python3
# fetch-fix-data.py
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
# wei.huang@noaa.gov
# 2025-02-26
# script to download a subset of FIX data to local machines.
import os
<<<<<<< HEAD
import time
import sys
import getopt
=======
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
import argparse
import subprocess
from pathlib import Path
import logging

# Create and configure logger
<<<<<<< HEAD
logging.basicConfig(filename="cfetch-fix-data.log",
=======
logging.basicConfig(filename="fetch-fix-data.log",
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
                    format='%(asctime)s %(message)s',
                    filemode='w')

# Creating an object
logger = logging.getLogger()

# Setting the threshold of logger to DEBUG
logger.setLevel(logging.DEBUG)

# ------------------------------------------------------------------------------


class FetchFIXdata():
<<<<<<< HEAD

    def __init__(self, atmgridarray=['C48'], ocngridarray=['500'],
                 fix_bucket=None, localdir=None, verbose=0):

=======
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
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
        # self.aws_fix_bucket = f's3://noaa-nws-global-pds/fix'
        self.aws_fix_bucket = fix_bucket
        self.aws_cp = f'aws --no-sign-request s3 cp'
        self.aws_sync = f'aws --no-sign-request s3 sync'

        self.atmgridarray = atmgridarray
        self.ocngridarray = ocngridarray
        self.localdir = localdir
<<<<<<< HEAD
        self.verbose = verbose

        if (os.path.isdir(localdir)):
            logger.info(f'Prepare to download FIX data for {atmgrid} and {ocngrid} to {localdir}')
        else:
            logger.info(f'local dir: <{localdir}> does not exist. Stop')
            sys.exit(-1)

        self.verdict = {}
=======
        self.fix_ver = fix_ver
        self.verbose = verbose

        logger.info(f'localdir: {localdir}')
        logger.info(f'fix_ver: {fix_ver}')
        logger.info(f'fix_buck: {fix_bucket}')
        logger.info(f'verbose: {verbose}')

        if (os.path.isdir(localdir)):
            logger.info(f'Prepare to download FIX data for {self.atmgridarray} and {self.ocngridarray} to {localdir}')
        else:
            logger.error(f'local dir: <{localdir}> does not exist. Stop')
            raise SystemExit

        if (os.path.isfile(fix_ver)):
            logger.info(f'Prepare to read FIX data for {self.atmgridarray} and {self.ocngridarray} to {fix_ver}')
        else:
            logger.error(f'File fix_ver: <{fix_ver}> does not exist. Stop')
            raise SystemExit

>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
        self.s3dict = {}
        self.s3dict['raworog'] = f'raw/orog'

        if (self.localdir.find('fix') < 0):
            self.targetdir = f'{self.localdir}/fix.subset'
        else:
            self.targetdir = self.localdir

<<<<<<< HEAD
    # --------------------------------------------------------------------------
    def update_s3dict(self):

        self.update_s3dick_grid_independent()
        self.add_grid_data()
=======
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
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234

        if (self.verbose):
            self.printinfo()

    # --------------------------------------------------------------------------
<<<<<<< HEAD
    def update_s3dick_grid_independent(self):

        for key in self.fix_ver_dict.keys():
            val = self.fix_ver_dict[key]
            if (key == 'aer_ver'):
                self.s3dict['aer'] = f'aer/{val}'
            elif (key == 'am_ver'):
                self.s3dict['am'] = f'am/{val}'
            elif (key == 'chem_ver'):
                self.s3dict['fimdata_chem'] = f'chem/{val}/fimdata_chem'
                self.s3dict['Emission_data'] = f'chem/{val}/Emission_data'
            elif (key == 'datm_ver'):
                self.s3dict['cfsr'] = f'datm/{val}/cfsr'
                self.s3dict['gefs'] = f'datm/{val}/gefs'
                self.s3dict['gfs'] = f'datm/{val}/gfs'
                self.s3dict['mom6'] = f'datm/{val}/mom6'
            elif (key == 'glwu_ver'):
                self.s3dict['glwu'] = f'glwu/{val}'
            elif (key == 'gsi_ver'):
                self.s3dict['gsi'] = f'gsi/{val}'
            elif (key == 'lut_ver'):
                self.s3dict['lut'] = f'lut/{val}'
            elif (key == 'mom6_ver'):
                self.s3dict['mom6post'] = f'mom6/{val}/post'
            elif (key == 'reg2grb2_ver'):
                self.s3dict['reg2grb2'] = f'reg2grb2/{val}'
            elif (key == 'sfc_climb_ver'):
                self.s3dict['sfc_climo'] = f'sfc_climo/{val}'
            elif (key == 'verif_ver'):
                self.s3dict['verif'] = f'verif/{val}'
            elif (key == 'wave_ver'):
                self.s3dict['wave'] = f'wave/{val}'

    # --------------------------------------------------------------------------
    def add_grid_data(self):

        for key in self.fix_ver_dict.keys():
            val = self.fix_ver_dict[key]
            if (key == 'orog_ver'):
                self.add_atmgrid2s3dict('orog', key, val)
            elif (key == 'ugwd_ver'):
                self.add_atmgrid2s3dict('ugwd', key, val)
            elif (key == 'mom6_ver'):
                self.add_ocngrid2s3dict('mom6', key, val)
            elif (key == 'cice_ver'):
                self.add_ocngrid2s3dict('cice', key, val)
            elif (key == 'cpl_ver'):
                self.add_cpl2s3dict('cpl', key, val)

    # --------------------------------------------------------------------------
    def add_atmgrid2s3dict(self, varname, key, val):

        for atmgrid in self.atmgridarray:
            newkey = f'{key}_{atmgrid}'
            self.s3dict[newkey] = f'{varname}/{val}/{atmgrid}'

    # -------------------------------------------------------------------------
    def add_ocngrid2s3dict(self, varname, key, val):

        for ocngrid in self.ocngridarray:
            newkey = f'{key}_{atmgrid}'
            self.s3dict[newkey] = f'{varname}/{val}/{ocngrid}'

    # -------------------------------------------------------------------------
    def add_cpl2s3dict(self, varname, key, val):

        for atmgrid in self.atmgridarray:
            for ocngrid in self.ocngridarray:
                newkey = f'{key}_a{atmgrid}o{ocngrid}'
                self.s3dict[newkey] = f'{varname}/{val}/a{atmgrid}o{ocngrid}'

    # -------------------------------------------------------------------------
    def printinfo(self):

        logger.info(f'Preparing to fetch')
        logger.info(f'ATM grid: {self.atmgridarray}')
        logger.info(f'ONC grid: {self.ocngridarray}')
        logger.info(f'From: {self.aws_fix_bucket}')
        logger.info(f'To: {self.targetdir}')
        for key in self.s3dict.keys():
            val = self.s3dict[key]
            logger.info(f'{key}: {val}')

    # -------------------------------------------------------------------------
    def fetchdata(self):

=======
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
            newkey = f'{key}_{ocngrid}'
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
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
        if (self.verbose):
            logger.info(f'Create local fix dir: {self.targetdir}')

        path = Path(self.targetdir)
        path.mkdir(parents=True, exist_ok=True)

        self.fetch_ugwp_limb_tau()

        for key in self.s3dict.keys():
<<<<<<< HEAD
            self.fetch_dir(self.s3dict[key])

    # -------------------------------------------------------------------------
    def fetch_dir(self, dir):

        remotedir = f'{self.aws_fix_bucket}/{dir}'
        localdir = f'{self.targetdir}/{dir}'
        cmd = f'{self.aws_sync} {remotedir} {localdir}'
        self.download_dir(cmd, localdir)

    # --------------------------------------------------------------------------
    def download_dir(self, cmd, localdir):

        # returned_value = os.system(cmd)  # returns the exit code in unix
        # logger.info('returned value:', returned_value)
=======
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
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234

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
<<<<<<< HEAD
                logger.info('returned value:', returned_value)

    # --------------------------------------------------------------------------
    def fetch_ugwp_limb_tau(self):

=======
                logger.info(f'returned value: {returned_value}')

    # --------------------------------------------------------------------------
    def fetch_ugwp_limb_tau(self):
        """download ugwp_limb_tau.nc
        """
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
        ugwd_ver = self.fix_ver_dict['ugwd_ver']
        ugwp_limb_tau_remotepath = f'{self.aws_fix_bucket}/ugwd/{ugwd_ver}/ugwp_limb_tau.nc'
        ugwp_limb_tau_localdir = f'{self.targetdir}/ugwd/{ugwd_ver}'
        filename = f'{ugwp_limb_tau_localdir}/ugwp_limb_tau.nc'
        path = Path(ugwp_limb_tau_localdir)
        path.mkdir(parents=True, exist_ok=True)
        cmd = f'{self.aws_cp} {ugwp_limb_tau_remotepath} {filename}'
<<<<<<< HEAD
        self.download_file(cmd, filename)

    # -------------------------------------------------------------------------
    def download_file(self, cmd, filename):

        # returned_value = os.system(cmd)  # returns the exit code in unix
        # logger.info('returned value:', returned_value)
=======

        # returned_value = os.system(cmd)  # returns the exit code in unix
        # if (self.verbose):
        #     logger.info(f'returned value: {returned_value}')
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234

        if (os.path.isfile(filename)):
            logger.info(f'{filename} already exist. skip')
        else:
            if (self.verbose):
                logger.info(cmd)
            logger.info(f'Downloading {filename}')
            returned_value = subprocess.call(cmd, shell=True)  # returns the exit code in unix
            if (self.verbose):
<<<<<<< HEAD
                logger.info('returned value:', returned_value)

    # --------------------------------------------------------------------------
    def set_fix_ver_from_gwhome(self, gwhome, verdict):

        fix_ver_file = f'{gwhome}/versions/fix.ver'
        self.fix_ver_dict = verdict
        if (os.path.isfile(fix_ver_file)):
            with open(fix_ver_file, "r") as file:
                for line in file.readlines():
                    if (line.find('export ') >= 0):
                        headstr, _, value = line.strip().partition('=')
                        exphead, _, key = headstr.partition(' ')
                        self.fix_ver_dict[key] = value
        else:
            logger.info(f'fix_ver_file: {fix_ver_file}s does not exist.')

    # ------------------------------------------------------------------------
    def set_default_fix_ver(self, verdict):

        self.fix_ver_dict = verdict

# -----------------------------------------------------------------------------


def namespace_to_dict(namespace):
    return {
        k: namespace_to_dict(v) if isinstance(v, argparse.Namespace) else v
        for k, v in vars(namespace).items()
    }

# ------------------------------------------------------------------------------


if __name__ == '__main__':

    atmgridlist = ['C48', 'C96', 'C192', 'C384', 'C768', 'C1152']
    ocngridlist = ['500', '100', '050', '025']
=======
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
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234

    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="increase output verbosity")
<<<<<<< HEAD
    parser.add_argument("-a", "--atmgrid", type=str, required=True,
                        help="ATM grid, like: C48, C96, C192, C384, C768, C1152")
    parser.add_argument("-o", "--ocngrid", type=str, required=True,
                        help="OCN grid, like: 500, 100, 050, 025")
    parser.add_argument("--localdir", type=str, required=True,
                        help="local directory to store FIX data subset")
    parser.add_argument("--gwhome", type=str, default='unknown',
                        help="GW home diretory where can find fix.ver")
    parser.add_argument("--fix_bucket", type=str, default='s3://noaa-nws-global-pds/fix',
                        help="S3 Bucket directory of FIX data")
    parser.add_argument("--aer_ver", type=str, default='20220805', help="AER version")
    parser.add_argument("--am_ver", type=str, default='20220805', help="AM version")
    parser.add_argument("--chem_ver", type=str, default='20220805', help="chem version")
    parser.add_argument("--cice_ver", type=str, default='20240416', help="cice version")
    parser.add_argument("--cpl_ver", type=str, default='20230526', help="cpl version")
    parser.add_argument("--datm_ver", type=str, default='20220805', help="datm version")
    parser.add_argument("--glwu_ver", type=str, default='20220805', help="glwu version")
    parser.add_argument("--gsi_ver", type=str, default='20240208', help="gsi version")
    parser.add_argument("--lut_ver", type=str, default='20220805', help="lut version")
    parser.add_argument("--mom6_ver", type=str, default='20240416', help="mom6 version")
    parser.add_argument("--orog_ver", type=str, default='20231027', help="orog version")
    parser.add_argument("--reg2grb2_ver", type=str, default='20220805', help="reg2grb2 version")
    parser.add_argument("--sfc_climo_ver", type=str, default='20220805', help="sfc_climo version")
    parser.add_argument("--ugwd_ver", type=str, default='20220805', help="ugwd version")
    parser.add_argument("--verif_ver", type=str, default='20220805', help="verif version")
    parser.add_argument("--wave_ver", type=str, default='20220805', help="wave version")
=======
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
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
    args = parser.parse_args()

    if args.verbose:
        logger.info(f"the atmgrid is {args.atmgrid}")
<<<<<<< HEAD
    else:
        logger.info(f"the atmgrid is {args.atmgrid}")
=======
        logger.info(f"the ocngrid is {args.ocngrid}")
        logger.info(f"the localdir is {args.localdir}")
        logger.info(f"the fix_file is {args.fix_ver}")
        logger.info(f"the s3 bucket is {args.fix_bucket}")
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234

    atmgrid = args.atmgrid
    if (atmgrid.find(',') > 0):
        atmgridarray = atmgrid.split(',')
    else:
        atmgridarray = [atmgrid]

    for grid in atmgridarray:
<<<<<<< HEAD
        if (grid not in atmgridlist):
            logger.info(f'atmgrid: {grid}')
            logger.info(f'is not in supported grids: {atmgridlist}')
            sys.exit(-1)
=======
        if (grid not in ATMGRIDLIST):
            logger.error(f'atmgrid: {grid}')
            logger.error(f'is not in supported grids: {ATMGRIDLIST}')
            raise SystemExit
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234

    ocngrid = args.ocngrid
    if (ocngrid.find(',') > 0):
        ocngridarray = ocngrid.split(',')
    else:
        ocngridarray = [ocngrid]

    for grid in ocngridarray:
<<<<<<< HEAD
        if (grid not in ocngridlist):
            logger.info(f'ocngrid: {grid}')
            logger.info(f'is not in supported grids: {ocngridlist}')
            sys.exit(-1)

    verdict = namespace_to_dict(args)

    # ------------------------------------------------------------------
    ffd = FetchFIXdata(atmgridarray=atmgridarray,
                       ocngridarray=ocngridarray,
                       fix_bucket=args.fix_bucket,
                       localdir=args.localdir, verbose=args.verbose)

    if (args.gwhome is None):
        ffd.set_default_fix_ver(verdict)
    else:
        ffd.set_fix_ver_from_gwhome(args.gwhome, verdict)

    ffd.update_s3dict()
    ffd.fetchdata()
=======
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
>>>>>>> 7fa952e0638f62539db47c96d37f64335c68e234
