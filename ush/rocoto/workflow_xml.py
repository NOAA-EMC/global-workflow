#!/usr/bin/env python

from datetime import datetime
from collections import OrderedDict
import rocoto
from applications import AppConfig
from workflow_tasks import get_wf_tasks


class RocotoXML:

    def __init__(self, app_config: AppConfig) -> None:

        # Initialize variables
        self.xml = None

        self._app_config = app_config

        self._base = self._app_config.configs['base']

        self.preamble = self._get_preamble()
        self.definitions = self._get_definitions()
        self.header = self._get_workflow_header()
        self.cycledefs = self._get_cycledefs()
        task_list = get_wf_tasks(app_config)
        self.tasks = ''.join(task_list)
        self.footer = self._get_workflow_footer()

        self.xml = self.assemble_xml()

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
                   '\t-->',
                   '']

        return '\n'.join(strings)

    def _get_definitions(self) -> str:
        """
        Create entities related to the experiment
        """

        entity = OrderedDict()

        entity['PSLOT'] = self._base['PSLOT']
        entity['SDATE'] = self._base['SDATE'].strftime('%Y%m%d%H%M')
        entity['EDATE'] = self._base['EDATE'].strftime('%Y%m%d%H%M')

        if self._app_config.mode in ['cycled']:
            entity['INTERVAL'] = self._base.get('INTERVAL', '06:00:00')
            if self._base['gfs_cyc'] in [1, 2, 4]:
                entity['SDATE_GFS'] = self._base['SDATE_GFS'].strftime('%Y%m%d%H%M')
                entity['EDATE_GFS'] = self._base['EDATE_GFS'].strftime('%Y%m%d%H%M')
                entity['INTERVAL_GFS'] = self._base['INTERVAL_GFS']
            entity['DMPDIR'] = self._base['DMPDIR']

        elif self._app_config.mode in ['forecast-only']:
            entity['INTERVAL'] = self._base.get('INTERVAL_GFS', '24:00:00')
            entity['CDUMP'] = self._base['CDUMP']
            entity['CASE'] = self._base['CASE']  # TODO - is this really used in the XML?
            entity['ICSDIR'] = self._base['ICSDIR']

        entity['EXPDIR'] = self._base['EXPDIR']
        entity['ROTDIR'] = self._base['ROTDIR']
        entity['RUN_ENVIR'] = self._base.get('RUN_ENVIR', 'emc')
        entity['HOMEgfs'] = self._base['HOMEgfs']
        entity['JOBS_DIR'] = self._base['BASE_JOB']
        entity['ARCHIVE_TO_HPSS'] = self._base.get('HPSSARCH', 'NO')

        entity['MAXTRIES'] = self._base.get('ROCOTO_MAXTRIES', 2)

        # Put them all in an XML key-value syntax
        strings = []
        for key, value in entity.items():
            strings.append('\t' + rocoto.create_entity(key, value))

        return '\n'.join(strings)

    def _get_workflow_header(self):
        """
        Create the workflow header block
        """

        scheduler = self._app_config.scheduler
        cyclethrottle = self._base.get('ROCOTO_CYCLETHROTTLE', 3)
        taskthrottle = self._base.get('ROCOTO_TASKTHROTTLE', 25)
        verbosity = self._base.get('ROCOTO_VERBOSITY', 10)

        strings = ['',
                   ']>',
                   '',
                   f'<workflow realtime="F" scheduler="{scheduler}" cyclethrottle="{cyclethrottle}" taskthrottle="{taskthrottle}">',
                   '',
                   f'\t<log verbosity="{verbosity}"><cyclestr>&EXPDIR;/logs/@Y@m@d@H.log</cyclestr></log>',
                   '',
                   '\t<!-- Define the cycles -->',
                   '']

        return '\n'.join(strings)

    def _get_cycledefs(self):

        cycledef_map = {'cycled': self._get_cycledefs_cycled,
                        'forecast-only': self._get_cycledefs_forecast_only}

        try:
            cycledefs = cycledef_map[self._app_config.mode]()
        except KeyError:
            raise KeyError(f'{self._app_config.mode} is not a valid application mode.\n' +
                           'Valid application modes are:\n' +
                           f'{", ".join(cycledef_map.keys())}')

        return cycledefs

    def _get_cycledefs_cycled(self):
        strings = ['\t<cycledef group="first">&SDATE;     &SDATE;     &INTERVAL;</cycledef>',
                   '\t<cycledef group="enkf" >&SDATE;     &EDATE;     &INTERVAL;</cycledef>',
                   '\t<cycledef group="gdas" >&SDATE;     &EDATE;     &INTERVAL;</cycledef>']
        if self._app_config.gfs_cyc != 0:
            strings.append('\t<cycledef group="gfs"  >&SDATE_GFS; &EDATE_GFS; &INTERVAL_GFS;</cycledef>')
            strings.append('')
            strings.append('')

        return '\n'.join(strings)

    def _get_cycledefs_forecast_only(self):
        strings = f'\t<cycledef group="cdump">&SDATE; &EDATE; &INTERVAL;</cycledef>'

        return strings

    @staticmethod
    def _get_workflow_footer():
        """
        Generate workflow footer
        """

        return '\n</workflow>\n'

    def assemble_xml(self) -> str:

        strings = [self.preamble,
                   self.definitions,
                   self.header,
                   self.cycledefs,
                   self.tasks,
                   self.footer]

        return ''.join(strings)

    def write(self, xml_file: str = None) -> None:

        if xml_file is None:
            xml_file = f"{self._base['EXPDIR']}/{self._base['PSLOT']}.xml"

        with open(xml_file, 'w') as fh:
            fh.write(self.xml)
