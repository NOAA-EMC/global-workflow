#!/usr/bin/env python3

from rocoto.workflow_xml import RocotoXML
from applications.applications import AppConfig
from wxflow import to_timedelta, timedelta_to_HMS
from typing import Dict


class GCAFSCycledRocotoXML(RocotoXML):

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:
        # Make sure we're using 'gcafs' as the run type
        # First ensure the keys exist before trying to access them
        if 'base' in app_config.configs:
            if app_config.configs['base']['RUN'] == 'gfs':
                app_config.configs['base']['RUN'] = 'gcafs'
            elif 'RUN' not in app_config.configs['base']:
                # If RUN is not defined, set it to 'gcafs'
                app_config.configs['base']['RUN'] = 'gcafs'
            else:
                app_config.configs['base']['RUN'] = 'gcafs'
        else:
            # If 'base' doesn't exist, initialize it with RUN set to 'gcafs'
            app_config.configs['base'] = {'RUN': 'gcafs'}

        super().__init__(app_config, rocoto_config)

    def get_cycledefs(self):
        sdate = self._base['SDATE']
        edate = self._base['EDATE']
        interval = to_timedelta(f"{self._base['assim_freq']}H")
        sdate_str = sdate.strftime("%Y%m%d%H%M")
        edate_str = edate.strftime("%Y%m%d%H%M")
        interval_str = timedelta_to_HMS(interval)
        strings = []
        strings.append(f'\t<cycledef group="gcafs_half">{sdate_str} {sdate_str} {interval_str}</cycledef>')
        sdate = sdate + interval
        sdate_str = sdate.strftime("%Y%m%d%H%M")
        strings.append(f'\t<cycledef group="gcafs">{sdate_str} {edate_str} {interval_str}</cycledef>')

        strings.append('')
        strings.append('')

        return '\n'.join(strings)
