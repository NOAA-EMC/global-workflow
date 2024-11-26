#!/usr/bin/env python3

import os
from distutils.spawn import find_executable
from datetime import datetime
from collections import OrderedDict
from typing import Dict
from applications.applications import AppConfig
from rocoto.workflow_tasks import get_wf_tasks
from wxflow import to_timedelta
import rocoto.rocoto as rocoto
from abc import ABC, abstractmethod


class RocotoXML(ABC):

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:

        self._app_config = app_config
        self.rocoto_config = rocoto_config

        # Use the first config.base (sourced with an arbitrary RUN)
        self._base = self._app_config.configs[next(iter(self._app_config.configs))]['base']
        self._base['interval_gdas'] = to_timedelta(f'{self._base["assim_freq"]}H')
        self._base['interval_gfs'] = to_timedelta(f'{self._base["INTERVAL_GFS"]}H')

        self.preamble = self._get_preamble()
        self.definitions = self._get_definitions()
        self.header = self._get_workflow_header()
        self.cycledefs = self.get_cycledefs()
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

        entity['ROTDIR'] = self._base['ROTDIR']
        entity['JOBS_DIR'] = self._base['BASE_JOB']

        entity['MAXTRIES'] = self.rocoto_config['maxtries']

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
        cyclethrottle = self.rocoto_config['cyclethrottle']
        taskthrottle = self.rocoto_config['taskthrottle']
        verbosity = self.rocoto_config['verbosity']

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

    @abstractmethod
    def get_cycledefs(self):
        pass

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
        self._write_xml(xml_file=xml_file)
        self._write_crontab(crontab_file=crontab_file)

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
                   f'MAILTO="{replyto}"'
                   ]
        # AWS need 'SHELL', and 'BASH_ENV' defined, or, the crontab job won't start.
        if os.environ.get('PW_CSP', None) in ['aws', 'azure', 'google']:
            strings.extend([f'SHELL="/bin/bash"',
                            f'BASH_ENV="/etc/bashrc"'])
        strings.extend([f'{cronintstr} {rocotorunstr}',
                        '#################################################################',
                        ''])

        if crontab_file is None:
            crontab_file = f"{expdir}/{pslot}.crontab"

        with open(crontab_file, 'w') as fh:
            fh.write('\n'.join(strings))

        return
