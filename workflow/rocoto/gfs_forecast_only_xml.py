#!/usr/bin/env python3

from rocoto.workflow_xml import RocotoXML
from applications.applications import AppConfig
from wxflow import to_timedelta, timedelta_to_HMS
from typing import Dict


class GFSForecastOnlyRocotoXML(RocotoXML):

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:
        super().__init__(app_config, rocoto_config)

    def get_cycledefs(self):
        sdate = self._base['SDATE_GFS']
        edate = self._base['EDATE']
        interval = self._app_config.interval_gfs
        strings = []
        strings.append(f'\t<cycledef group="gfs">{sdate.strftime("%Y%m%d%H%M")} {edate.strftime("%Y%m%d%H%M")} {timedelta_to_HMS(interval)}</cycledef>')

        date2 = sdate + interval
        if date2 <= edate:
            strings.append(f'\t<cycledef group="gfs_seq">{date2.strftime("%Y%m%d%H%M")} {edate.strftime("%Y%m%d%H%M")} {timedelta_to_HMS(interval)}</cycledef>')

        strings.append('')
        strings.append('')

        return '\n'.join(strings)
