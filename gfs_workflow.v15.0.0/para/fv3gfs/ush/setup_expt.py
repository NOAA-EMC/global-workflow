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

if __name__ == '__main__':

    description = '''Setup files and directories to start a GFS parallel.
Create EXPDIR, copy config files
Create COMROT experiment directory structure,
link initial condition files from $ICSDIR to $COMROT'''

    parser = ArgumentParser(description=description,formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--pslot',help='parallel experiment name',type=str,required=True)
    parser.add_argument('--configdir',help='full path to directory containing the config files',type=str,required=True)
    parser.add_argument('--idate',help='date of initial conditions',type=str,required=False,default='2016100100')
    parser.add_argument('--icsdir',help='full path to initial condition directory',type=str,required=False,default='/scratch4/NCEPDEV/da/noscrub/Rahul.Mahajan/ICS')
    parser.add_argument('--resdet',help='resolution of the deterministic model forecast',type=int,required=False,default=384)
    parser.add_argument('--resens',help='resolution of the ensemble model forecast',type=int,required=False,default=192)
    parser.add_argument('--comrot',help='full path to COMROT',type=str,required=False,default=None)
    parser.add_argument('--expdir',help='full path to EXPDIR',type=str,required=False,default=None)
    parser.add_argument('--nens',help='number of ensemble members',type=int,required=False,default=80)
    parser.add_argument('--cdump',help='CDUMP to start the experiment',type=str,required=False,default='gdas')

    args = parser.parse_args()

    pslot = args.pslot
    configdir = args.configdir
    idate = datetime.strptime(args.idate,'%Y%m%d%H')
    icsdir = args.icsdir
    resdet = args.resdet
    resens = args.resens
    comrot = args.comrot if args.comrot is None else os.path.join(args.comrot,pslot)
    expdir = args.expdir if args.expdir is None else os.path.join(args.expdir,pslot)
    nens = args.nens
    cdump = args.cdump

    idatestr = idate.strftime('%Y%m%d%H')
    cymd = idate.strftime('%Y%m%d')
    chh = idate.strftime('%H')

    rc = 0

    if not os.path.exists(icsdir):
        print 'Initial conditions do not exist in %s' % icsdir
        sys.exit(1)

    create_comrot = False if comrot is None else True
    if create_comrot and os.path.exists(comrot):
        print
        print 'COMROT already exists in %s' % comrot
        print
        overwrite_comrot = raw_input('Do you wish to over-write COMROT [y/N]: ')
        create_comrot = True if overwrite_comrot in ['y', 'yes', 'Y', 'YES'] else False

    create_expdir = False if expdir is None else True
    if create_expdir and os.path.exists(expdir):
        print
        print 'EXPDIR already exists in %s' % expdir
        print
        overwrite_expdir = raw_input('Do you wish to over-write EXPDIR [y/N]: ')
        create_expdir = True if overwrite_expdir in ['y', 'yes', 'Y', 'YES'] else False

    # Create COMROT directory
    if create_comrot:
        if os.path.exists(comrot): shutil.rmtree(comrot)
        os.makedirs(comrot)

        # Link ensemble member initial conditions
        enkfdir = 'enkf.%s.%s/%s' % (cdump,cymd,chh)
        os.makedirs(os.path.join(comrot,enkfdir))
        for i in range(1,nens+1):
            os.makedirs(os.path.join(comrot,enkfdir,'mem%03d'%i))
            os.symlink(os.path.join(icsdir,idatestr,'C%d'%resens,'mem%03d'%i,'INPUT'),os.path.join(comrot,enkfdir,'mem%03d'%i,'INPUT'))

        # Link deterministic initial conditions
        detdir = '%s.%s/%s' % (cdump,cymd,chh)
        os.makedirs(os.path.join(comrot,detdir))
        os.symlink(os.path.join(icsdir,idatestr,'C%d'%resdet,'control','INPUT'),os.path.join(comrot,detdir,'INPUT'))

        # Link bias correction and radiance diagnostics files
        for fname in ['abias','abias_pc','abias_air','radstat']:
            os.symlink(os.path.join(icsdir,idatestr,'%s.t%sz.%s'%(cdump,chh,fname)),os.path.join(comrot,detdir,'%s.t%sz.%s'%(cdump,chh,fname)))

    # Create EXP directory and copy config
    if create_expdir:
        if os.path.exists(expdir): shutil.rmtree(expdir)
        os.makedirs(expdir)
        configs = glob.glob('%s/config.*' % configdir)
        for config in configs:
            shutil.copy(config,expdir)

    sys.exit(0)
