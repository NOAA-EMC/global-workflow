#!/usr/bin/env python

from datetime import datetime
from collections import OrderedDict
from hosts import Host
import rocoto


def get_preamble():
    """
    Generate preamble for XML
    """

    strings = []

    strings.append('<?xml version="1.0"?>')
    strings.append('<!DOCTYPE workflow')
    strings.append('[')
    strings.append('\t<!--')
    strings.append('\tPROGRAM')
    strings.append('\t\tMain workflow manager for Global Forecast System')
    strings.append('')
    strings.append('\tNOTES:')
    strings.append(f'\t\tThis workflow was automatically generated at {datetime.now()}')
    strings.append('\t-->')

    return '\n'.join(strings)


def get_definitions(base: dict, mode: str) -> str:
    """
    Create entities related to the experiment
    """

    host = Host()

    scheduler = host.scheduler

    entity = OrderedDict()

    entity['PSLOT'] = base['PSLOT']
    entity['SDATE'] = base['SDATE'].strftime('%Y%m%d%H%M')
    entity['EDATE'] = base['EDATE'].strftime('%Y%m%d%H%M')

    if mode in ['cycled']:
        entity['INTERVAL'] = '06:00:00'
        if base['gfs_cyc'] in [1, 2, 4]:
            entity['SDATE_GFS'] = base['SDATE_GFS'].strftime('%Y%m%d%H%M')
            entity['EDATE_GFS'] = base['EDATE_GFS'].strftime('%Y%m%d%H%M')
            entity['INTERVAL_GFS'] = base['INTERVAL_GFS']
        entity['DMPDIR'] = base['DMPDIR']

    elif mode in ['forecast-only']:
        entity['INTERVAL'] = base.get('INTERVAL_GFS', '24:00:00')
        entity['CDUMP'] = base['CDUMP']
        entity['CASE'] = base['CASE']  # TODO - is this really used in the XML?
        entity['ICSDIR'] = base['ICSDIR']

    entity['EXPDIR'] = base['EXPDIR']
    entity['ROTDIR'] = base['ROTDIR']
    entity['RUN_ENVIR'] = base.get('RUN_ENVIR', 'emc')
    entity['HOMEgfs'] = base['HOMEgfs']
    entity['JOBS_DIR'] = base['BASE_JOB']
    entity['ACCOUNT'] = base['ACCOUNT']
    entity['QUEUE'] = base['QUEUE']
    entity['QUEUE_SERVICE'] = base['QUEUE_SERVICE']
    entity['SCHEDULER'] = scheduler
    if scheduler in ['slurm']:
        entity['PARTITION_BATCH'] = base['PARTITION_BATCH']
        entity['PARTITION_SERVICE'] = base['PARTITION_SERVICE']
    entity['ARCHIVE_TO_HPSS'] = base.get('HPSSARCH', 'NO')
    entity['CYCLETHROTTLE'] = base.get('ROCOTO_CYCLETHROTTLE', 3)
    entity['TASKTHROTTLE'] = base.get('ROCOTO_TASKTHROTTLE', 25)
    entity['MAXTRIES'] = base.get('ROCOTO_MAXTRIES', 2)
    entity['VERBOSITY'] = base.get('ROCOTO_VERBOSITY', 10)

    # Put them all in an XML key-value syntax
    strings = []
    for key, value in entity.items():
        strings.append('\t' + rocoto.create_entity(key, value))

    return '\n'.join(strings)
