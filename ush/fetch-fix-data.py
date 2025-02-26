#!/usr/bin/env python
# cfetch-fix-data.py
# wei.huang@noaa.gov
# 2025-02-26
# script to download a subset of FIX data to local machines.
import os
import time
import sys
import getopt
import subprocess
from pathlib import Path

# ------------------------------------------------------------------------------
class FetchFIXdata():


    def __init__(self, atmgridarray=['C48'], ocngridarray=['500'], localdir=None, verbose=0):

        self.aws_fix_bucket= 's3://noaa-nws-global-pds/fix'
        self.aws_cp='aws --no-sign-request s3 cp'
        self.aws_sync='aws --no-sign-request s3 sync'

        self.atmgridarray=atmgridarray
        self.ocngridarray=ocngridarray
        self.localdir=localdir
        self.verbose=verbose

        # if (os.path.isdir(localdir)):
        #    print('Prepare to download FIX data for %s and %s to %s' %(atmgrid, ocngrid, localdir))
        # else:
        #    print(f'local dir: <{localdir}> does not exist. Stop')
        #    sys.exit(-1)

        self.verdict={}
        self.s3dict={}
        self.s3dict['raworog']='raw/orog'

        if (self.localdir.find('fix') < 0):
            self.targetdir=f'{self.localdir}/fix.subset'
        else:
            self.targetdir=self.localdir

    # --------------------------------------------------------------------------
    def update_s3dict(self):

        self.update_s3dick_grid_independent()
        self.add_grid_data()

        if (self.verbose):
            self.printinfo()

    # --------------------------------------------------------------------------
    def update_s3dick_grid_independent(self):

        for key in self.fix_ver_dict.keys():
            val=self.fix_ver_dict[key]
            if (key == 'aer_ver'):
                self.s3dict['aer']=f'aer/{val}'
            elif (key == 'am_ver'):
                self.s3dict['am']=f'am/{val}'
            elif (key == 'chem_ver'):
                self.s3dict['fimdata_chem']=f'chem/{val}/fimdata_chem'
                self.s3dict['Emission_data']=f'chem/{val}/Emission_data'
            elif (key == 'datm_ver'):
                self.s3dict['cfsr']=f'datm/{val}/cfsr'
                self.s3dict['gefs']=f'datm/{val}/gefs'
                self.s3dict['gfs']=f'datm/{val}/gfs'
                self.s3dict['mom6']=f'datm/{val}/mom6'
            elif (key == 'glwu_ver'):
                self.s3dict['glwu']=f'glwu/{val}'
            elif (key == 'gsi_ver'):
                self.s3dict['gsi']=f'gsi/{val}'
            elif (key == 'lut_ver'):
                self.s3dict['lut']=f'lut/{val}'
            elif (key == 'mom6_ver'):
                self.s3dict['mom6post']=f'mom6/{val}/post'
            elif (key == 'reg2grb2_ver'):
                self.s3dict['reg2grb2']=f'reg2grb2/{val}'
            elif (key == 'sfc_climb_ver'):
                self.s3dict['sfc_climo']=f'sfc_climo/{val}'
            elif (key == 'verif_ver'):
                self.s3dict['verif']=f'verif/{val}'
            elif (key == 'wave_ver'):
                self.s3dict['wave']=f'wave/{val}'

    # --------------------------------------------------------------------------
    def add_grid_data(self):

        for key in self.fix_ver_dict.keys():
            val=self.fix_ver_dict[key]
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
            newkey=f'{key}_{atmgrid}'
            self.s3dict[newkey]=f'{varname}/{val}/{atmgrid}'

    # -------------------------------------------------------------------------
    def add_ocngrid2s3dict(self, varname, key, val):

        for ocngrid in self.ocngridarray:
            newkey=f'{key}_{atmgrid}'
            self.s3dict[newkey]=f'{varname}/{val}/{ocngrid}'

    # -------------------------------------------------------------------------
    def add_cpl2s3dict(self, varname, key, val):

        for atmgrid in self.atmgridarray:
            for ocngrid in self.ocngridarray:
                newkey=f'{key}_a{atmgrid}o{ocngrid}'
                self.s3dict[newkey]=f'{varname}/{val}/a{atmgrid}o{ocngrid}'

    # -------------------------------------------------------------------------
    def printinfo(self):

        print(f'Preparing to fetch')
        print(f'ATM grid: {self.atmgridarray}')
        print(f'ONC grid: {self.ocngridarray}')
        print(f'From: {self.aws_fix_bucket}')
        print(f'To: {self.targetdir}')
        for key in self.s3dict.keys():
            val=self.s3dict[key]
            print(f'{key}: {val}')

    # -------------------------------------------------------------------------
    def fetchdata(self):

        if (self.verbose):
            print('Create local fix dir: {self.targetdir}')

        path=Path(self.targetdir)
        path.mkdir(parents=True, exist_ok=True)

        self.fetch_ugwp_limb_tau()
        
        for key in self.s3dict.keys():
            self.fetch_dir(self.s3dict[key])

    # -------------------------------------------------------------------------
    def fetch_dir(self, dir):

        remotedir=f'{self.aws_fix_bucket}/{dir}'
        localdir=f'{self.targetdir}/{dir}'
        cmd=f'{self.aws_sync} {remotedir} {localdir}'
        self.download_dir(cmd, localdir)

    # --------------------------------------------------------------------------
    def download_dir(self, cmd, localdir):

        # returned_value=os.system(cmd)  # returns the exit code in unix
        # print('returned value:', returned_value)

        if (os.path.isdir(localdir)):
            print(f'{localdir} already exist. skip'
        else:
            parentdir, dirname=os.path.split(localdir)
            if (self.verbose):
                print(f'Create local {parentdir} dir:')
            path=Path(parentdir)
            path.mkdir(parents=True, exist_ok=True)
            if (self.verbose):
                print(cmd)
            print(f'Downloading {localdir}')
            returned_value=subprocess.call(cmd, shell=True)  # returns the exit code in unix
            if (self.verbose):
                print('returned value:', returned_value)

    # --------------------------------------------------------------------------
    def fetch_ugwp_limb_tau(self):

        ugwp_limb_tau_remotepath=f'{self.aws_fix_bucket}/ugwd/{self.fix_ver_dict['ugwd_ver']}/ugwp_limb_tau.nc'
        ugwp_limb_tau_localdir=f'{self.targetdir}/ugwd/{self.fix_ver_dict['ugwd_ver']}'
        filename=f'{ugwp_limb_tau_localdir}/ugwp_limb_tau.nc'
        path=Path(ugwp_limb_tau_localdir)
        path.mkdir(parents=True, exist_ok=True)
        cmd=f'{self.aws_cp} {ugwp_limb_tau_remotepath} {filename}'
        self.download_file(cmd, filename)

    # -------------------------------------------------------------------------
    def download_file(self, cmd, filename):

       # returned_value=os.system(cmd)  # returns the exit code in unix
       # print('returned value:', returned_value)

        if (os.path.isfile(filename)):
            print(f'{filename} already exist. skip')
        else:
            if (self.verbose):
                print(cmd)
            print(f'Downloading {filename}')
            returned_value=subprocess.call(cmd, shell=True)  # returns the exit code in unix
            if (self.verbose):
                print('returned value:', returned_value)

    # --------------------------------------------------------------------------
    def set_fix_ver_from_gwhome(self, gwhome, verdict):

        fix_ver_file=f'{gwhome}/versions/fix.ver'
        self.fix_ver_dict=verdict
        if (os.path.isfile(fix_ver_file)):
            with open(fix_ver_file, "r") as file:
                for line in file.readlines():
                    if (line.find('export ') >= 0):
                        headstr, _, value=line.strip().partition('=')
                        exphead, _, key=headstr.partition(' ')
                        self.fix_ver_dict[key]=value
        else:
            print(f'fix_ver_file: {ix_ver_file}s does not exist.')

    # ------------------------------------------------------------------------
    def set_default_fix_ver(self, verdict):

        self.fix_ver_dict=verdict

# -----------------------------------------------------------------------------
def print_usage(verdict):


    print('Usage: python fetch-fix-data.py \\')
    print('       --atmgrid=AtmospericGrid (for multiple grids, separate with ",") \\')
    print('       --ocngrid=OceanGrid (for multiple grids, separate with ",") \\')
    print('       --localdir=Your-local-fix-dir \\')
    print('       [options]')
    print('options are:')
    print('\t--gwhome=xxxx (Global-Workflow directory)')

    for key in verdict.keys():
        print(f'\t--{key}=yyyymmdd  default: {verdict[key]}')

# ------------------------------------------------------------------------------
if __name__ == '__main__':


    atmgridlist=['C48', 'C96', 'C192', 'C384', 'C768', 'C1152']
    ocngridlist=['500', '100', '050', '025']

    verbose=0
    atmgrid='C48'
    ocngrid='500'
    localdir='/contrib/global-workflow-shared-data'

    # default fix-version
    verdict={}
    verdict['aer_ver']='20220805'
    verdict['am_ver']='20220805'
    verdict['chem_ver']='20220805'
    verdict['cice_ver']='20240416'
    verdict['cpl_ver']='20230526'
    verdict['datm_ver']='20220805'
    verdict['glwu_ver']='20220805'
    verdict['gsi_ver']='20240208'
    verdict['lut_ver']='20220805'
    verdict['mom6_ver']='20240416'
    verdict['orog_ver']='20231027'
    verdict['reg2grb2_ver']='20220805'
    verdict['sfc_climo_ver']='20220805'
    verdict['ugwd_ver']='20240624'
    verdict['verif_ver']='20220805'
    verdict['wave_ver']='20240105'

    gwhome=None

    opts, args=getopt.getopt(sys.argv[1:], '', ['help', 'atmgrid=', 'ocngrid=',
                                                'verbose=', 'localdir=',
                                                'gwhome=',
                                                'aer_ver=',
                                                'am_ver=',
                                                'chem_ver=',
                                                'cice_ver=',
                                                'cpl_ver=',
                                                'datm_ver=',
                                                'glwu_ver=',
                                                'gsi_ver=',
                                                'lut_ver=',
                                                'mom6_ver=',
                                                'orog_ver=',
                                                'reg2grb2_ver=',
                                                'sfc_climo_ver=',
                                                'ugwd_ver=',
                                                'verif_ver=',
                                                'wave_ver='])
    for o, a in opts:
        # print(f'o: {o}, a: {a}')
        if o in ['--help']:
            print_usage(verdict)
            sys.exit(0)
        elif o in ['--verbose']:
            verbose=int(a)
        elif o in ['--atmgrid']:
            atmgrid=a
        elif o in ['--ocngrid']:
            ocngrid=a
        elif o in ['--localdir']:
            localdir=a
        elif o in ['--gwhome']:
            gwhome=a
        else:
            _, vername=o.split('--')
            print(f'vername: <{vername}>')
            verdict[vername]=a

    if (atmgrid.find(',') > 0):
        atmgridarray=atmgrid.split(',')
    else:
        atmgridarray=[atmgrid]

    for grid in atmgridarray:
        if (grid not in atmgridlist):
            print('atmgrid: ', grid)
            print('is not in supported grids: ', atmgridlist)
            print_usage(verdict)
            sys.exit(-1)

    if (ocngrid.find(',') > 0):
        ocngridarray=ocngrid.split(',')
    else:
        ocngridarray=[ocngrid]

    for grid in ocngridarray:
        if (grid not in ocngridlist):
            print('ocngrid: ', grid)
            print('is not in supported grids: ', ocngridlist)
            print_usage(verdict)
            sys.exit(-1)

    # ------------------------------------------------------------------
    ffd=FetchFIXdata(atmgridarray=atmgridarray,
                     ocngridarray=ocngridarray,
                     localdir=localdir, verbose=verbose)

    if (gwhome is None):
        ffd.set_default_fix_ver(verdict)
    else:
        ffd.set_fix_ver_from_gwhome(gwhome, verdict)

    ffd.update_s3dict()

    ffd.fetchdata()
