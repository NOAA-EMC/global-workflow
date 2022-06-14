#!/usr/bin/env python

from datetime import datetime
from collections import OrderedDict
from hosts import Host
import rocoto
from applications import Application


class RocotoXML:

    def __init__(self, app: Application) -> None:

        self._app = app

        self._base = self._app.job_configs['base']

        self.preamble = self._get_preamble
        self.definitions = self._get_definitions()
        self.header = self._get_workflow_header
        self.cycledefs = self._get_cycledefs()
        self.footer = self._get_workflow_footer

    @staticmethod
    def _get_preamble():
        """
        Generate preamble for XML
        """

        strings = ['<?xml version="1.0"?>',
                   '<!DOCTYPE workflow',
                   '[',
                   '\t<!--',
                   '\tPROGRAM',
                   '\t\tMain workflow manager for Global Forecast System',
                   '',
                   '\tNOTES:',
                   f'\t\tThis workflow was automatically generated at {datetime.now()}',
                   '\t-->']

        return '\n'.join(strings)

    def _get_definitions(self) -> str:
        """
        Create entities related to the experiment
        """

        host = Host()

        scheduler = host.scheduler

        entity = OrderedDict()

        entity['PSLOT'] = self._base['PSLOT']
        entity['SDATE'] = self._base['SDATE'].strftime('%Y%m%d%H%M')
        entity['EDATE'] = self._base['EDATE'].strftime('%Y%m%d%H%M')

        if self._app.mode in ['cycled']:
            entity['INTERVAL'] = '06:00:00'
            if self._base['gfs_cyc'] in [1, 2, 4]:
                entity['SDATE_GFS'] = self._base['SDATE_GFS'].strftime('%Y%m%d%H%M')
                entity['EDATE_GFS'] = self._base['EDATE_GFS'].strftime('%Y%m%d%H%M')
                entity['INTERVAL_GFS'] = self._base['INTERVAL_GFS']
            entity['DMPDIR'] = self._base['DMPDIR']

        elif self._app.mode in ['forecast-only']:
            entity['INTERVAL'] = self._base.get('INTERVAL_GFS', '24:00:00')
            entity['CDUMP'] = self._base['CDUMP']
            entity['CASE'] = self._base['CASE']  # TODO - is this really used in the XML?
            entity['ICSDIR'] = self._base['ICSDIR']

        entity['EXPDIR'] = self._base['EXPDIR']
        entity['ROTDIR'] = self._base['ROTDIR']
        entity['RUN_ENVIR'] = self._base.get('RUN_ENVIR', 'emc')
        entity['HOMEgfs'] = self._base['HOMEgfs']
        entity['JOBS_DIR'] = self._base['BASE_JOB']
        entity['ACCOUNT'] = self._base['ACCOUNT']
        entity['QUEUE'] = self._base['QUEUE']
        entity['QUEUE_SERVICE'] = self._base['QUEUE_SERVICE']
        entity['SCHEDULER'] = scheduler
        if scheduler in ['slurm']:
            entity['PARTITION_BATCH'] = self._base['PARTITION_BATCH']
            entity['PARTITION_SERVICE'] = self._base['PARTITION_SERVICE']
        entity['ARCHIVE_TO_HPSS'] = self._base.get('HPSSARCH', 'NO')
        entity['CYCLETHROTTLE'] = self._base.get('ROCOTO_CYCLETHROTTLE', 3)
        entity['TASKTHROTTLE'] = self._base.get('ROCOTO_TASKTHROTTLE', 25)
        entity['MAXTRIES'] = self._base.get('ROCOTO_MAXTRIES', 2)
        entity['VERBOSITY'] = self._base.get('ROCOTO_VERBOSITY', 10)

        # Put them all in an XML key-value syntax
        strings = []
        for key, value in entity.items():
            strings.append('\t' + rocoto.create_entity(key, value))

        return '\n'.join(strings)

    @staticmethod
    def _get_workflow_header():
        """
        Create the workflow header block
        """

        strings = ['',
                   ']>',
                   '',
                   '<workflow realtime="F" scheduler="&SCHEDULER;" cyclethrottle="&CYCLETHROTTLE;" taskthrottle="&TASKTHROTTLE;">',
                   '',
                   '\t<log verbosity="10"><cyclestr>&EXPDIR;/logs/@Y@m@d@H.log</cyclestr></log>',
                   '',
                   '\t<!-- Define the cycles -->']

        return '\n'.join(strings)

    def _get_cycledefs(self):

        cycledef_map = {'cycled': self._get_cycledefs_cycled,
                        'forecast-only': self._get_cycledefs_forecast_only}

        try:
            cycledef_map[self._app.mode]()
        except KeyError:
            raise TypeError(f'{self._app.mode} is not a valid application.' +
                            'Current plot types supported are:\n' +
                            f'{" | ".join(cycledef_map.keys())}"')

    def _get_cycledefs_cycled(self):
        strings = ['\t<cycledef group="first">&SDATE;     &SDATE;     06:00:00</cycledef>',
                   '\t<cycledef group="enkf" >&SDATE;     &EDATE;     06:00:00</cycledef>',
                   '\t<cycledef group="gdas" >&SDATE;     &EDATE;     06:00:00</cycledef>']
        if self._app.gfs_cyc != 0:
            strings.append('\t<cycledef group="gfs"  >&SDATE_GFS; &EDATE_GFS; &INTERVAL_GFS;</cycledef>')

        return '\n'.join(strings)

    @staticmethod
    def _get_cycledefs_forecast_only():
        strings = f'\t<cycledef group="cdump">&SDATE; &EDATE; &INTERVAL;</cycledef>'

        return strings

    @staticmethod
    def _get_workflow_footer():
        """
        Generate workflow footer
        """

        return '\n</workflow>\n'


def create_xml():
    return None
