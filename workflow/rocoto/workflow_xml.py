#!/usr/bin/env python3

import os
from distutils.spawn import find_executable
from datetime import datetime
from collections import OrderedDict
from applications import AppConfig
from rocoto.workflow_tasks import get_wf_tasks
import rocoto.rocoto as rocoto


class RocotoXML:

    def __init__(self, app_config: AppConfig) -> None:

        self._app_config = app_config

        self._base = self._app_config.configs['base']

        self.preamble = self._get_preamble()
        self.definitions = self._get_definitions()
        self.header = self._get_workflow_header()
        self.cycledefs = self._get_cycledefs()
        task_list = get_wf_tasks(app_config)
        self.tasks = '\n'.join(task_list)
        self.footer = self._get_workflow_footer()

        self.xml = self._assemble_xml()

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

        if self._app_config.mode in ['forecast-only']:
            entity['ICSDIR'] = self._base['ICSDIR']

        entity['ROTDIR'] = self._base['ROTDIR']
        entity['JOBS_DIR'] = self._base['BASE_JOB']

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
        if self._app_config.do_efsoi:
            cyclethrottle = self._base.get('ROCOTO_CYCLETHROTTLE', 7)
        else:
            cyclethrottle = self._base.get('ROCOTO_CYCLETHROTTLE', 3)
        taskthrottle = self._base.get('ROCOTO_TASKTHROTTLE', 25)
        verbosity = self._base.get('ROCOTO_VERBOSITY', 10)

        expdir = self._base['EXPDIR']

        strings = ['',
                   ']>',
                   '',
                   f'<workflow realtime="F" scheduler="{scheduler}" cyclethrottle="{cyclethrottle}" taskthrottle="{taskthrottle}">',
                   '',
                   f'\t<log verbosity="{verbosity}"><cyclestr>{expdir}/logs/@Y@m@d@H.log</cyclestr></log>',
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
        sdate = self._base['SDATE'].strftime('%Y%m%d%H%M')
        edate = self._base['EDATE'].strftime('%Y%m%d%H%M')
        interval = self._base.get('INTERVAL', '06:00:00')
        strings = [f'\t<cycledef group="gdas" >{sdate} {edate} {interval}</cycledef>\n']

        if self._app_config.gfs_cyc != 0:
            sdate_gfs = self._base['SDATE_GFS'].strftime('%Y%m%d%H%M')
            edate_gfs = self._base['EDATE_GFS'].strftime('%Y%m%d%H%M')
            interval_gfs = self._base['INTERVAL_GFS']
            strings.append(f'\t<cycledef group="gfs"  >{sdate_gfs} {edate_gfs} {interval_gfs}</cycledef>')
            strings.append('')
            strings.append('')

        return '\n'.join(strings)

    def _get_cycledefs_forecast_only(self):
        sdate = self._base['SDATE'].strftime('%Y%m%d%H%M')
        edate = self._base['EDATE'].strftime('%Y%m%d%H%M')
        interval = self._base.get('INTERVAL_GFS', '24:00:00')
        cdump = self._base['CDUMP']
        strings = f'\t<cycledef group="{cdump}">{sdate} {edate} {interval}</cycledef>\n\n'

        return strings

    @staticmethod
    def _get_workflow_footer():
        """
        Generate workflow footer
        """

        return '\n</workflow>\n'

    def _assemble_xml(self) -> str:

        strings = [self.preamble,
                   self.definitions,
                   self.header,
                   self.cycledefs,
                   self.tasks,
                   self.footer]

        return ''.join(strings)

    def write(self, xml_file: str = None, crontab_file: str = None):
        self._write_xml(xml_file = xml_file)
        self._write_crontab(crontab_file = crontab_file)

    def _write_xml(self, xml_file: str = None) -> None:

        expdir = self._base['EXPDIR']
        pslot = self._base['PSLOT']

        if xml_file is None:
            xml_file = f"{expdir}/{pslot}.xml"

        with open(xml_file, 'w') as fh:
            fh.write(self.xml)

    def _write_crontab(self, crontab_file: str = None, cronint: int = 5) -> None:
        """
        Create crontab to execute rocotorun every cronint (5) minutes
        """

        # No point creating a crontab if rocotorun is not available.
        rocotoruncmd = find_executable('rocotorun')
        if rocotoruncmd is None:
            print('Failed to find rocotorun, crontab will not be created')
            return

        expdir = self._base['EXPDIR']
        pslot = self._base['PSLOT']

        rocotorunstr = f'{rocotoruncmd} -d {expdir}/{pslot}.db -w {expdir}/{pslot}.xml'
        cronintstr = f'*/{cronint} * * * *'

        try:
            replyto = os.environ['REPLYTO']
        except KeyError:
            replyto = ''

        strings = ['',
                   f'#################### {pslot} ####################',
                   f'MAILTO="{replyto}"',
                   f'{cronintstr} {rocotorunstr}',
                   '#################################################################',
                   '']

        if crontab_file is None:
            crontab_file = f"{expdir}/{pslot}.crontab"

        with open(crontab_file, 'w') as fh:
            fh.write('\n'.join(strings))

        return
