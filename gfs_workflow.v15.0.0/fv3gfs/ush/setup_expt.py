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
global expdir, configdir, comrot, pslot, resdet, resens, nens, cdump, idate, edate, gfs_cyc

machines = ['THEIA', 'WCOSS_C']


def create_EXPDIR():

    os.makedirs(expdir)
    configs = glob.glob('%s/config.*' % configdir)
    if len(configs) == 0:
        msg = 'no config files found in %s' % configdir
        raise IOError(msg)
    for config in configs:
        shutil.copy(config, expdir)

    return


def create_COMROT():

    idatestr = idate.strftime('%Y%m%d%H')
    cymd = idate.strftime('%Y%m%d')
    chh = idate.strftime('%H')

    os.makedirs(comrot)

    # Link ensemble member initial conditions
    enkfdir = 'enkf.%s.%s/%s' % (cdump, cymd, chh)
    os.makedirs(os.path.join(comrot, enkfdir))
    for i in range(1, nens+1):
        os.makedirs(os.path.join(comrot, enkfdir, 'mem%03d'%i))
        os.symlink(os.path.join(icsdir, idatestr, 'C%d'%resens, 'mem%03d'%i, 'INPUT'),
                   os.path.join(comrot, enkfdir, 'mem%03d'%i, 'INPUT'))

    # Link deterministic initial conditions
    detdir = '%s.%s/%s' % (cdump, cymd, chh)
    os.makedirs(os.path.join(comrot, detdir))
    os.symlink(os.path.join(icsdir, idatestr, 'C%d'%resdet, 'control', 'INPUT'),
               os.path.join(comrot,detdir,'INPUT'))

    # Link bias correction and radiance diagnostics files
    for fname in ['abias','abias_pc','abias_air','radstat']:
        os.symlink(os.path.join(icsdir, idatestr, '%s.t%sz.%s' % (cdump, chh, fname)),
                   os.path.join(comrot, detdir, '%s.t%sz.%s' % (cdump, chh, fname)))

    return


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
    lines = [l.replace('@SDATE@', idate.strftime('%Y%m%d%H')) for l in lines]
    lines = [l.replace('@EDATE@', edate.strftime('%Y%m%d%H')) for l in lines]
    if expdir is not None:
        lines = [l.replace('@EXPDIR@', os.path.dirname(expdir)) for l in lines]
    if comrot is not None:
        lines = [l.replace('@ROTDIR@', os.path.dirname(comrot)) for l in lines]
    lines = [l.replace('@CASECTL@', 'C%d'%resdet) for l in lines]
    lines = [l.replace('@CASEENS@', 'C%d'%resens) for l in lines]
    lines = [l.replace('@NMEM_ENKF@', '%d'%nens) for l in lines]
    lines = [l.replace('@gfs_cyc@', '%d'%gfs_cyc) for l in lines]
    lines_to_remove = ['ICSDIR']
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
Create EXPDIR, copy config files
Create COMROT experiment directory structure,
link initial condition files from $ICSDIR to $COMROT'''

    parser = ArgumentParser(description=description,formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--machine', help='machine name', type=str, choices=machines, default='WCOSS_C', required=False)
    parser.add_argument('--pslot', help='parallel experiment name', type=str, required=False, default='test')
    parser.add_argument('--resdet', help='resolution of the deterministic model forecast', type=int, required=False, default=384)
    parser.add_argument('--resens', help='resolution of the ensemble model forecast', type=int, required=False, default=192)
    parser.add_argument('--comrot', help='full path to COMROT', type=str, required=False, default=None)
    parser.add_argument('--expdir', help='full path to EXPDIR', type=str, required=False, default=None)
    parser.add_argument('--idate', help='starting date of experiment, initial conditions must exist!', type=str, required=True)
    parser.add_argument('--edate', help='end date experiment', type=str, required=True)
    parser.add_argument('--icsdir', help='full path to initial condition directory', type=str, required=True)
    parser.add_argument('--configdir', help='full path to directory containing the config files', type=str, required=True)
    parser.add_argument('--nens', help='number of ensemble members', type=int, required=False, default=80)
    parser.add_argument('--cdump', help='CDUMP to start the experiment', type=str, required=False, default='gdas')
    parser.add_argument('--gfs_cyc', help='GFS cycles to run', type=int, choices=[0, 1, 2, 4], default=1, required=False)

    args = parser.parse_args()

    machine = args.machine
    pslot = args.pslot
    configdir = args.configdir
    idate = datetime.strptime(args.idate,'%Y%m%d%H')
    edate = datetime.strptime(args.edate,'%Y%m%d%H')
    icsdir = args.icsdir
    resdet = args.resdet
    resens = args.resens
    comrot = args.comrot if args.comrot is None else os.path.join(args.comrot,pslot)
    expdir = args.expdir if args.expdir is None else os.path.join(args.expdir,pslot)
    nens = args.nens
    cdump = args.cdump
    gfs_cyc = args.gfs_cyc

    if not os.path.exists(icsdir):
        msg = 'Initial conditions do not exist in %s' % icsdir
        raise IOError(msg)

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
        create_COMROT()

    # EXP directory
    create_expdir = True
    if os.path.exists(expdir):
        print
        print 'EXPDIR already exists in %s' % expdir
        print
        overwrite_expdir = raw_input('Do you wish to over-write EXPDIR [y/N]: ')
        create_expdir = True if overwrite_expdir in ['y', 'yes', 'Y', 'YES'] else False
        if create_expdir: shutil.rmtree(expdir)

    if create_expdir:
        create_EXPDIR()
        edit_baseconfig()

    sys.exit(0)
