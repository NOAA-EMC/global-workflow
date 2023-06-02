#!/usr/bin/env python3

from rocoto.workflow_xml import RocotoXML
from applications.applications import AppConfig
from pygw.timetools import to_timedelta
from typing import Dict


# Copy of GFSForecastOnlyRocotoXML for now, other than changing cycledef names from 'gfs' to 'gefs'
#   If it remains this way, we can consolidate into a single forecast-only class
class GEFSRocotoXML(RocotoXML):

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:
        super().__init__(app_config, rocoto_config)

    def get_cycledefs(self):
        sdate = self._base['SDATE']
        edate = self._base['EDATE']
        interval = self._base.get('INTERVAL_GFS', '24:00:00')
        strings = []
        strings.append(f'\t<cycledef group="gefs">{sdate.strftime("%Y%m%d%H%M")} {edate.strftime("%Y%m%d%H%M")} {interval}</cycledef>')

        sdate = sdate + to_timedelta(interval)
        if sdate <= edate:
            strings.append(f'\t<cycledef group="gefs_seq">{sdate.strftime("%Y%m%d%H%M")} {edate.strftime("%Y%m%d%H%M")} {interval}</cycledef>')

        strings.append('')
        strings.append('')

        return '\n'.join(strings)
