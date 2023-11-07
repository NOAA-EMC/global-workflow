#!/usr/bin/env python3

from rocoto.workflow_xml import RocotoXML
from applications.applications import AppConfig
from wxflow import to_timedelta, timedelta_to_HMS
from typing import Dict


class GFSCycledRocotoXML(RocotoXML):

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:
        super().__init__(app_config, rocoto_config)

    def get_cycledefs(self):
        sdate = self._base['SDATE']
        edate = self._base['EDATE']
        interval = to_timedelta(f"{self._base['assim_freq']}H")
        strings = []
        strings.append(f'\t<cycledef group="gdas_half">{sdate.strftime("%Y%m%d%H%M")} {sdate.strftime("%Y%m%d%H%M")} {timedelta_to_HMS(interval)}</cycledef>')
        sdate = sdate + interval
        strings.append(f'\t<cycledef group="gdas">{sdate.strftime("%Y%m%d%H%M")} {edate.strftime("%Y%m%d%H%M")} {timedelta_to_HMS(interval)}</cycledef>')

        if self._app_config.gfs_cyc != 0:
            sdate_gfs = self._base['SDATE_GFS']
            edate_gfs = self._base['EDATE_GFS']
            interval_gfs = self._base['INTERVAL_GFS']
            strings.append(f'\t<cycledef group="gfs">{sdate_gfs.strftime("%Y%m%d%H%M")} {edate_gfs.strftime("%Y%m%d%H%M")} {timedelta_to_HMS(interval_gfs)}</cycledef>')

            sdate_gfs = sdate_gfs + interval_gfs
            if sdate_gfs <= edate_gfs:
                strings.append(f'\t<cycledef group="gfs_seq">{sdate_gfs.strftime("%Y%m%d%H%M")} {edate_gfs.strftime("%Y%m%d%H%M")} {timedelta_to_HMS(interval_gfs)}</cycledef>')

        strings.append('')
        strings.append('')

        return '\n'.join(strings)
