#! /usr/bin/env python3
f'This script requires python 3.6 or later'

import os, logging
from contextlib import suppress

logger=logging.getLogger('create_comrot')

def make_link(src,tgt):
    logger.debug(f'{src}: symlink {tgt}')
    with suppress(FileNotFoundError): os.unlink(tgt)
    if not os.path.exists(src):
        logger.warning(f'{src}: link target does not exist')
    os.symlink(src,tgt)

def make_dir(dir):
    logger.debug(f'{dir}: makedirs')
    with suppress(FileExistsError): os.makedirs(dir)

def create_COMROT(conf):
    cdump = conf.case.IC_CDUMP
    icsdir = conf.case.IC_DIR
    comrot = conf.places.ROTDIR
    resens = conf.fv3_enkf_settings.CASE[1:]
    resdet = conf.fv3_gfs_settings.CASE[1:]
    idate = conf.case.SDATE
    detdir = f'{cdump}.{idate:%Y%m%d}/{idate:%H}'
    nens = conf.data_assimilation.NMEM_ENKF
    enkfdir = f'enkf{cdump}.{idate:%Y%m%d}/{idate:%H}'
    idatestr = f'{idate:%Y%m%d%H}'

    logger.info(f'Input conditions: {icsdir}')

    make_dir(os.path.join(comrot,enkfdir))
    make_dir(os.path.join(comrot, detdir))

    logger.info(f'Workflow COM root: {comrot}')

    # Link ensemble member initial conditions
    for i in range(1, nens + 1):
        memdir=os.path.join(comrot,enkfdir,f'mem{i:03d}')
        make_dir(memdir)
        src=os.path.join(icsdir, idatestr, f'C{resens}',f'mem{i:03d}','INPUT')
        tgt=os.path.join(comrot, enkfdir, f'mem{i:03d}', 'INPUT')
        make_link(src,tgt)

    # Link deterministic initial conditions
    src=os.path.join(icsdir, idatestr, f'C{resdet}', 'control', 'INPUT')
    tgt=os.path.join(comrot, detdir, 'INPUT')
    make_link(src,tgt)

    # Link bias correction and radiance diagnostics files
    for fname in ['abias', 'abias_pc', 'abias_air', 'radstat']:
        file=f'{cdump}.t{idate:%H}z.{fname}'
        src=os.path.join(icsdir, idatestr, file)
        tgt=os.path.join(comrot, detdir, file)
        make_link(src,tgt)
