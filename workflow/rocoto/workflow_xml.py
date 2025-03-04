#!/usr/bin/env python3

import os
import stat
from distutils.spawn import find_executable
from datetime import datetime
from collections import OrderedDict
from typing import Dict
from applications.applications import AppConfig
from rocoto.workflow_tasks import get_wf_tasks
from wxflow import which, to_timedelta, mkdir
import rocoto.rocoto as rocoto
from abc import ABC, abstractmethod
from hosts import Host


class RocotoXML(ABC):

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:

        self._app_config = app_config
        self.rocoto_config = rocoto_config

        # Use the first config.base (sourced with an arbitrary RUN)
        self._base = self._app_config.configs[next(iter(self._app_config.configs))]['base']
        self._base['interval_gdas'] = to_timedelta(f'{self._base["assim_freq"]}H')
        self._base['interval_gfs'] = to_timedelta(f'{self._base["INTERVAL_GFS"]}H')

        # Collect info needed to write an scrontab file
        self.host_info = Host().info
        self.use_scrontab = self.host_info.get("USE_SCRONTAB", False)
        # Replace ACCOUNT with whatever is in config.base
        self.host_info.ACCOUNT = self._base['ACCOUNT']
        self.HOMEgfs = self._base['HOMEgfs']
        self.expdir = self._base['EXPDIR']
        self.pslot = self._base['PSLOT']

        # Get sections need to construct the XML
        self.preamble = self._get_preamble()
        self.definitions = self._get_definitions()
        self.header = self._get_workflow_header()
        self.cycledefs = self.get_cycledefs()
        task_list = get_wf_tasks(app_config)
        self.tasks = '\n'.join(task_list)
        self.footer = self._get_workflow_footer()

        # If we are running scrontab, check if the rocotorc file has the right entries
        if self.use_scrontab:
            self._check_rocotorc()

        # Construct the XML
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

        entity['PSLOT'] = self.pslot

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

        strings = ['',
                   ']>',
                   '',
                   f'<workflow realtime="F" scheduler="{scheduler}" cyclethrottle="{cyclethrottle}" taskthrottle="{taskthrottle}">',
                   '',
                   f'\t<log verbosity="{verbosity}"><cyclestr>{self.expdir}/logs/@Y@m@d@H.log</cyclestr></log>',
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

        if xml_file is None:
            xml_file = f"{self.expdir}/{self.pslot}.xml"

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

        rocotorunstr = f'{rocotoruncmd} -d {self.expdir}/{self.pslot}.db -w {self.expdir}/{self.pslot}.xml'
        cronintstr = f'*/{cronint} * * * *'

        replyto = os.environ.get('REPLYTO', "")

        crontab_strings = [
            '',
            f'#################### {self.pslot} ####################'
        ]

        # Construct the crontab or scrontab
        if self.use_scrontab:
            # The slurm crontab needs an SCRON entry that calls a script
            # envery n minutes.  That script will actually run rocoto.
            account = self.host_info.ACCOUNT
            partition = self.host_info.get("PARTITION_CRON", None) or self.host_info.PARTITION_SERVICE
            log_dir = os.path.join(self.expdir, "logs")
            mkdir(log_dir)
            crontab_strings.extend([
                f'#SCRON --partition={partition}',
                f'#SCRON --account={account}',
                f'#SCRON --mail-user={replyto}',
                f'#SCRON --job-name={self.pslot}_scron',
                f'#SCRON --output={self.expdir}/logs/scron.log',
                '#SCRON --time=00:10:00',
                '#SCRON --dependency=singleton'
            ])

            # Now write the script that actually runs rocotorun
            cron_cmd = f"{self.expdir}/{self.pslot}.scron.sh"
            with open(cron_cmd, "w") as script_fh:
                script_fh.write(
                    "#!/usr/bin/env bash\n" +
                    "set -x\n" +
                    f"source {self.HOMEgfs}/workflow/gw_setup.sh" + "\n" +
                    rocotorunstr + "\n"
                )

            # Make the script executable
            mode = os.stat(cron_cmd)
            os.chmod(cron_cmd, mode.st_mode | stat.S_IEXEC)
        else:
            cron_cmd = rocotorunstr
            crontab_strings.extend([
                f'MAILTO="{replyto}"'
            ])

        crontab_strings.extend([
            f'{cronintstr} {cron_cmd}',
            '#################################################################',
            ''
        ])

        # AWS need 'SHELL', and 'BASH_ENV' defined, or, the crontab job won't start.
        if os.environ.get('PW_CSP', None) in ['aws', 'azure', 'google']:
            crontab_strings.extend([
                'SHELL="/bin/bash"',
                'BASH_ENV="/etc/bashrc"'
            ])

        if crontab_file is None:
            crontab_file = f"{self.expdir}/{self.pslot}.crontab"

        # Write out the crontab/scrontab file
        with open(crontab_file, 'w') as fh:
            fh.write('\n'.join(crontab_strings))

        return

    def _check_rocotorc(self):

        rocotorun = which("rocotorun")

        if rocotorun is None:
            raise FileNotFoundError("Could not find the rocotorun executable.  Make sure you have the module loaded!")

        version = rocotorun("--version", output=str, error=str).split()[-1].strip()

        homedir = os.path.expanduser("~")
        rocotorc_file = os.path.join(homedir, ".rocoto", version, "rocotorc")

        if not os.path.isfile(rocotorc_file):
            raise FileNotFoundError(
                "Could not find the rocotorc file!\n"
                f"Please create '{rocotorc_file}' following the documentation at" "\n"
                "https://global-workflow.readthedocs.io/en/latest/configure.html"
            )

        with open(rocotorc_file) as rc_f:
            if ":BatchQueueServer: false" not in rc_f.read():
                raise ValueError(
                    f"':BatchQueueServer: false' should be written to {rocotorc_file}, but it is not!" "\n"
                    "Please follow the documentation guide here:\n"
                    "https://global-workflow.readthedocs.io/en/latest/configure.html"
                )
