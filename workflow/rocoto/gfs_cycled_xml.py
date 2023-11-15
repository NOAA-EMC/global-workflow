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
        sdate_str = sdate.strftime("%Y%m%d%H%M")
        edate_str = edate.strftime("%Y%m%d%H%M")
        interval_str = timedelta_to_HMS(interval)
        strings = []
        strings.append(f'\t<cycledef group="gdas_half">{sdate_str} {sdate_str} {interval_str}</cycledef>')
        sdate = sdate + interval
        sdate_str = sdate.strftime("%Y%m%d%H%M")
        strings.append(f'\t<cycledef group="gdas">{sdate_str} {edate_str} {interval_str}</cycledef>')

        if self._app_config.gfs_cyc != 0:
            sdate_gfs = self._base['SDATE_GFS']
            edate_gfs = self._base['EDATE_GFS']
            interval_gfs = self._base['INTERVAL_GFS']
            sdate_gfs_str = sdate_gfs.strftime("%Y%m%d%H%M")
            edate_gfs_str = edate_gfs.strftime("%Y%m%d%H%M")
            interval_gfs_str = timedelta_to_HMS(interval_gfs)
            strings.append(f'\t<cycledef group="gfs">{sdate_gfs_str} {edate_gfs_str} {interval_gfs_str}</cycledef>')

            sdate_gfs = sdate_gfs + interval_gfs
            sdate_gfs_str = sdate_gfs.strftime("%Y%m%d%H%M")
            if sdate_gfs <= edate_gfs:
                strings.append(f'\t<cycledef group="gfs_seq">{sdate_gfs_str} {edate_gfs_str} {interval_gfs_str}</cycledef>')

        strings.append('')
        strings.append('')

        return '\n'.join(strings)
