#!/usr/bin/env python3

from rocoto.workflow_xml import RocotoXML
from applications.applications import AppConfig
from wxflow import to_timedelta, timedelta_to_HMS
from typing import Dict


# Copy of GFSForecastOnlyRocotoXML for now, other than changing cycledef names from 'gfs' to 'gefs'
#   If it remains this way, we can consolidate into a single forecast-only class
class GEFSRocotoXML(RocotoXML):

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:
        super().__init__(app_config, rocoto_config)

    def get_cycledefs(self):
        sdate = self._base['SDATE']
        edate = self._base['EDATE']
        interval = self._base.get('INTERVAL_GFS', to_timedelta('24H'))
        sdate_str = sdate.strftime("%Y%m%d%H%M")
        edate_str = edate.strftime("%Y%m%d%H%M")
        interval_str = timedelta_to_HMS(interval)
        strings = []
        strings.append(f'\t<cycledef group="gefs">{sdate_str} {edate_str} {interval_str}</cycledef>')

        sdate = sdate + interval
        if sdate <= edate:
            sdate_str = sdate.strftime("%Y%m%d%H%M")
            strings.append(f'\t<cycledef group="gefs_seq">{sdate_str} {edate_str} {interval_str}</cycledef>')

        strings.append('')
        strings.append('')

        return '\n'.join(strings)
