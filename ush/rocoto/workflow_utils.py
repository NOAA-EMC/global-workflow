#!/usr/bin/env python

"""
    Module containing functions all workflow setups require
"""
import os
from distutils.spawn import find_executable
from datetime import timedelta
from typing import Dict, Any


def check_expdir(cmd_expdir, cfg_expdir):

    if not os.path.samefile(cmd_expdir, cfg_expdir):
        print('MISMATCH in experiment directories!')
        print(f'config.base: EXPDIR = {cfg_expdir}')
        print(f'input arg:     --expdir = {cmd_expdir}')
        raise ValueError('Abort!')


def get_gfs_interval(gfs_cyc: int) -> str:
    """
    return interval in hours based on gfs_cyc
    """

    gfs_internal_map = {'0': None, '1': '24:00:00', '2': '12:00:00', '4': '06:00:00'}

    try:
        return gfs_internal_map[str(gfs_cyc)]
    except KeyError:
        raise KeyError(f'Invalid gfs_cyc = {gfs_cyc}')


def get_gfs_cyc_dates(base: Dict[str, Any]) -> Dict[str, Any]:
    """
    Generate GFS dates from experiment dates and gfs_cyc choice
    """

    base_out = base.copy()

    gfs_cyc = base['gfs_cyc']
    sdate = base['SDATE']
    edate = base['EDATE']
    base_out['INTERVAL'] = '06:00:00'  # Cycled interval is 6 hours

    interval_gfs = get_gfs_interval(gfs_cyc)

    # Set GFS cycling dates
    hrinc = 0
    hrdet = 0
    if gfs_cyc == 0:
        return base_out
    elif gfs_cyc == 1:
        hrinc = 24 - sdate.hour
        hrdet = edate.hour
    elif gfs_cyc == 2:
        if sdate.hour in [0, 12]:
            hrinc = 12
        elif sdate.hour in [6, 18]:
            hrinc = 6
        if edate.hour in [6, 18]:
            hrdet = 6
    elif gfs_cyc == 4:
        hrinc = 6
    sdate_gfs = sdate + timedelta(hours=hrinc)
    edate_gfs = edate - timedelta(hours=hrdet)
    if sdate_gfs > edate:
        print('W A R N I N G!')
        print('Starting date for GFS cycles is after Ending date of experiment')
        print(f'SDATE = {sdate.strftime("%Y%m%d%H")},     EDATE = {edate.strftime("%Y%m%d%H")}')
        print(f'SDATE_GFS = {sdate_gfs.strftime("%Y%m%d%H")}, EDATE_GFS = {edate_gfs.strftime("%Y%m%d%H")}')
        gfs_cyc = 0

    base_out['gfs_cyc'] = gfs_cyc
    base_out['SDATE_GFS'] = sdate_gfs
    base_out['EDATE_GFS'] = edate_gfs
    base_out['INTERVAL_GFS'] = interval_gfs

    fhmax_gfs = {}
    for hh in ['00', '06', '12', '18']:
        fhmax_gfs[hh] = base.get(f'FHMAX_GFS_{hh}', base.get('FHMAX_GFS_00', 120))
    base_out['FHMAX_GFS'] = fhmax_gfs

    return base_out


def create_crontab(base, cronint=5):
    """
    Create crontab to execute rocotorun every cronint (5) minutes
    """

    # No point creating a crontab if rocotorun is not available.
    rocotoruncmd = find_executable('rocotorun')
    if rocotoruncmd is None:
        print('Failed to find rocotorun, crontab will not be created')
        return

    rocotorunstr = f'''{rocotoruncmd} -d {base['EXPDIR']}/{base['PSLOT']}.db -w {base['EXPDIR']}/{base['PSLOT']}.xml'''
    cronintstr = f'*/{cronint} * * * *'

    try:
        replyto = os.environ['REPLYTO']
    except KeyError:
        replyto = ''

    strings = ['',
               f'#################### {base["PSLOT"]} ####################',
               f'MAILTO="{replyto}"',
               f'{cronintstr} {rocotorunstr}',
               '#################################################################',
               '']

    with open(os.path.join(base['EXPDIR'], f'{base["PSLOT"]}.crontab'), 'w') as fh:
        fh.write('\n'.join(strings))

    return
