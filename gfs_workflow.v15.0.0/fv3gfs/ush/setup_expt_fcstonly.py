#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

import os
import sys
import glob
import shutil
from datetime import datetime
from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter


global machines
global machine, pslot, sdate, edate, expdir, comrot, res, configdir, gfs_cyc


machines = ['THEIA', 'WCOSS_C']


def edit_baseconfig():

    base_config = '%s/config.base' % expdir

    # make a copy of the default before editing
    shutil.copy(base_config, base_config+'.default')

    fh = open(base_config,'r')
    lines = fh.readlines()
    fh.close()

    lines = [l.replace('@MACHINE@', machine.upper()) for l in lines]

    # Only keep current machine information, remove others
    # A better way would be to cat from another machine specific file
    for m in machines:
        if m in [machine.upper()]:
            continue
        ind_begin = lines.index('# BEGIN: %s\n' % m)
        ind_end = lines.index('# END: %s\n' % m)
        lines = lines[:ind_begin] + lines[ind_end+1:]

    lines = [l.replace('@PSLOT@', pslot) for l in lines]
    lines = [l.replace('@SDATE@', sdate.strftime('%Y%m%d%H')) for l in lines]
    lines = [l.replace('@EDATE@', edate.strftime('%Y%m%d%H')) for l in lines]
    if expdir is not None:
        lines = [l.replace('@EXPDIR@', os.path.dirname(expdir)) for l in lines]
    if comrot is not None:
        lines = [l.replace('@ROTDIR@', os.path.dirname(comrot)) for l in lines]
    lines = [l.replace('@CASECTL@', 'C%d'%res) for l in lines]
    lines = [l.replace('@gfs_cyc@', '%d'%gfs_cyc) for l in lines]
    lines_to_remove = ['@CASEENS@', '@NMEM_ENKF@', 'RECENTER_ENKF']
    for l in lines_to_remove:
        lines = [ x for x in lines if "%s" % l not in x ]
    fh = open(base_config,'w')
    fh.writelines(lines)
    fh.close()

    print ''
    print 'EDITED:  %s/config.base as per user input.' % expdir
    print 'DEFAULT: %s/config.base.default is for reference only.' % expdir
    print 'Please verify and delete the default file before proceeding.'
    print ''

    return


if __name__ == '__main__':

    description = '''Setup files and directories to start a GFS parallel.
Create EXPDIR, copy config files and edit config.base
Create empty COMROT experiment directory'''

    parser = ArgumentParser(description=description,formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--machine', help='machine name', type=str.upper, choices=machines, default='WCOSS_C', required=False)
    parser.add_argument('--pslot', help='parallel experiment name', type=str, required=False, default='test')
    parser.add_argument('--configdir', help='full path to directory containing the config files', type=str, required=True)
    parser.add_argument('--sdate', help='starting date of experiment', type=str, required=False, default='2016100100')
    parser.add_argument('--edate', help='ending date of experiment', type=str, required=False, default='2016100200')
    parser.add_argument('--res', help='resolution of the model', type=int, required=False, default=384)
    parser.add_argument('--comrot', help='full path to COMROT', type=str, required=True)
    parser.add_argument('--expdir', help='full path to EXPDIR', type=str, required=True)
    parser.add_argument('--gfs_cyc', help='GFS cycles to run', type=int, choices=[0, 1, 2, 4], default=1, required=False)

    args = parser.parse_args()

    machine = args.machine
    pslot = args.pslot
    expdir = os.path.join(args.expdir, pslot)
    comrot = os.path.join(args.comrot, pslot)
    sdate = datetime.strptime(args.sdate,'%Y%m%d%H')
    edate = datetime.strptime(args.edate,'%Y%m%d%H')
    res = args.res
    configdir = args.configdir
    gfs_cyc = args.gfs_cyc

    if machine not in machines:
        print 'supported machines are ' % ' '.join(machines)
        print 'machine %s is unsupported, ABORT!' % machine
        sys.exit(1)

    # COMROT directory
    create_comrot = True
    if os.path.exists(comrot):
        print
        print 'COMROT already exists in %s' % comrot
        print
        overwrite_comrot = raw_input('Do you wish to over-write COMROT [y/N]: ')
        create_comrot = True if overwrite_comrot in ['y', 'yes', 'Y', 'YES'] else False
        if create_comrot: shutil.rmtree(comrot)

    if create_comrot:
        os.makedirs(comrot)

    # EXPDIR directory
    create_expdir = True
    if os.path.exists(expdir):
        print
        print 'EXPDIR already exists in %s' % expdir
        print
        overwrite_expdir = raw_input('Do you wish to over-write EXPDIR [y/N]: ')
        create_expdir = True if overwrite_expdir in ['y', 'yes', 'Y', 'YES'] else False
        if create_expdir: shutil.rmtree(expdir)

    if create_expdir:
        os.makedirs(expdir)
        configs = glob.glob('%s/config.*' % configdir)
        if len(configs) == 0:
            msg = 'no config files found in %s' % configdir
            raise IOError(msg)
        for config in configs:
            shutil.copy(config, expdir)
        edit_baseconfig()

    sys.exit(0)
