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
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


global machines
global expdir, configdir, comrot, pslot, res, idate, edate, gfs_cyc


machines = ['THEIA', 'WCOSS_C', 'WCOSS_DELL_P3']


def makedirs_if_missing(d):
    if not os.path.exists(d):
        os.makedirs(d)


def create_EXPDIR():

    makedirs_if_missing(expdir)
    configs = glob.glob('%s/config.*' % configdir)
    if len(configs) == 0:
        msg = 'no config files found in %s' % configdir
        raise IOError(msg)
    for config in configs:
        shutil.copy(config, expdir)

    return


def create_COMROT():

    makedirs_if_missing(comrot)

    return


def edit_baseconfig():

    base_config = '%s/config.base' % expdir

    here = os.path.dirname(__file__)
    top = os.path.abspath(os.path.join(os.path.abspath(here), '../..'))

    # make a copy of the default before editing
    shutil.copy(base_config, base_config + '.default')

    print '\nSDATE = %s\nEDATE = %s' % (idate, edate)
    with open(base_config + '.default', 'rt') as fi:
        with open(base_config + '.new', 'wt') as fo:
            for line in fi:
                line = line.replace('@MACHINE@', machine.upper()) \
                    .replace('@PSLOT@', pslot) \
                    .replace('@SDATE@', idate.strftime('%Y%m%d%H')) \
                    .replace('@EDATE@', edate.strftime('%Y%m%d%H')) \
                    .replace('@CASECTL@', 'C%d' % res) \
                    .replace('@HOMEgfs@', top) \
                    .replace('@gfs_cyc@', '%d' % gfs_cyc)
                if expdir is not None:
                    line = line.replace('@EXPDIR@', os.path.dirname(expdir))
                if comrot is not None:
                    line = line.replace('@ROTDIR@', os.path.dirname(comrot))
                line = line.replace('@ICSDIR@', os.path.join(os.path.dirname(comrot), 'FV3ICS'))
                fo.write(line)
    os.unlink(base_config)
    os.rename(base_config + '.new', base_config)

    print ''
    print 'EDITED:  %s/config.base as per user input.' % expdir
    print 'DEFAULT: %s/config.base.default is for reference only.' % expdir
    print 'Please verify and delete the default file before proceeding.'
    print ''

    return


if __name__ == '__main__':

    description = '''Setup files and directories to start a GFS parallel.
Create EXPDIR, copy config files
Create COMROT experiment directory structure'''

    parser = ArgumentParser(description=description, formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--pslot', help='parallel experiment name', type=str, required=False, default='test')
    parser.add_argument('--res', help='resolution of the model forecast', type=int, required=False, default=192)
    parser.add_argument('--comrot', help='full path to COMROT', type=str, required=False, default=None)
    parser.add_argument('--expdir', help='full path to EXPDIR', type=str, required=False, default=None)
    parser.add_argument('--idate', help='starting date of experiment, initial conditions must exist!', type=str, required=True)
    parser.add_argument('--edate', help='end date experiment', type=str, required=True)
    parser.add_argument('--configdir', help='full path to directory containing the config files', type=str, required=False, default=None)
    parser.add_argument('--gfs_cyc', help='GFS cycles to run', type=int, choices=[0, 1, 2, 4], default=1, required=False)

    args = parser.parse_args()

    if os.path.exists('/scratch3'):
        machine = 'THEIA'
    elif os.path.exists('/gpfs') and os.path.exists('/etc/SuSE-release'):
        machine = 'WCOSS_C'
    elif os.path.exists('/gpfs/dell2'):
        machine = 'WCOSS_DELL_P3'
    else:
        print 'workflow is currently only supported on: %s' % ' '.join(machines)
        raise NotImplementedError('Cannot auto-detect platform, ABORT!')

    configdir = args.configdir
    if not configdir:
        configdir = os.path.abspath(os.path.dirname(__file__) + '/../parm/config')

    pslot = args.pslot
    idate = datetime.strptime(args.idate, '%Y%m%d%H')
    edate = datetime.strptime(args.edate, '%Y%m%d%H')
    res = args.res
    comrot = args.comrot if args.comrot is None else os.path.join(args.comrot, pslot)
    expdir = args.expdir if args.expdir is None else os.path.join(args.expdir, pslot)
    gfs_cyc = args.gfs_cyc

    # COMROT directory
    create_comrot = True
    if os.path.exists(comrot):
        print
        print 'COMROT already exists in %s' % comrot
        print
        overwrite_comrot = raw_input('Do you wish to over-write COMROT [y/N]: ')
        create_comrot = True if overwrite_comrot in ['y', 'yes', 'Y', 'YES'] else False
        if create_comrot:
            shutil.rmtree(comrot)

    if create_comrot:
        create_COMROT()

    # EXP directory
    create_expdir = True
    if os.path.exists(expdir):
        print
        print 'EXPDIR already exists in %s' % expdir
        print
        overwrite_expdir = raw_input('Do you wish to over-write EXPDIR [y/N]: ')
        create_expdir = True if overwrite_expdir in ['y', 'yes', 'Y', 'YES'] else False
        if create_expdir:
            shutil.rmtree(expdir)

    if create_expdir:
        create_EXPDIR()
        edit_baseconfig()

    sys.exit(0)
