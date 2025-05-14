#!/usr/bin/env python3

"""
GFS forecast-only XML generator module.

This module provides functionality to generate Rocoto XML workflow configurations
for GFS forecast-only runs. It handles cycle definitions and specific task configurations
needed for the GFS workflow.
"""

from rocoto.workflow_xml import RocotoXML
from applications.applications import AppConfig
from wxflow import to_timedelta, timedelta_to_HMS
from typing import Dict


class GFSForecastOnlyRocotoXML(RocotoXML):
    """
    Rocoto XML generator for GFS forecast-only workflows.

    This class handles the generation of Rocoto XML configuration for GFS
    forecast-only mode, including cycle definitions and workflow scheduling.

    Parameters
    ----------
    app_config : AppConfig
        Application configuration object containing GFS settings
    rocoto_config : Dict
        Dictionary containing Rocoto-specific configuration
    """

    def __init__(self, app_config: AppConfig, rocoto_config: Dict) -> None:
        """
        Initialize GFS forecast-only Rocoto XML generator.

        Parameters
        ----------
        app_config : AppConfig
            Application configuration object containing GFS settings
        rocoto_config : Dict
            Dictionary containing Rocoto-specific configuration
        """
        super().__init__(app_config, rocoto_config)

    def get_cycledefs(self):
        """
        Generate cycle definition strings for Rocoto XML.

        This method creates the cycle definitions for GFS forecasts and verification
        cycles based on the configured start dates, end dates, and intervals.

        Returns
        -------
        str
            Concatenated cycle definition strings formatted for Rocoto XML
        """
        sdate_gfs = self._base['SDATE_GFS']
        edate_gfs = self._base['EDATE']
        interval_gfs = self._base['interval_gfs']
        strings = []
        sdate_gfs_str = sdate_gfs.strftime("%Y%m%d%H%M")
        edate_gfs_str = edate_gfs.strftime("%Y%m%d%H%M")
        interval_gfs_str = timedelta_to_HMS(interval_gfs)
        strings.append(f'\t<cycledef group="gfs">{sdate_gfs_str} {edate_gfs_str} {interval_gfs_str}</cycledef>')

        date2_gfs = sdate_gfs + interval_gfs
        if date2_gfs <= edate_gfs:
            date2_gfs_str = date2_gfs.strftime("%Y%m%d%H%M")
            strings.append(f'\t<cycledef group="gfs_seq">{date2_gfs_str} {edate_gfs_str} {interval_gfs_str}</cycledef>')

        if self._base['DO_METP']:
            if interval_gfs < to_timedelta('24H'):
                # Run verification at 18z, no matter what if there is more than one gfs per day
                sdate_metp = sdate_gfs.replace(hour=18)
                edate_metp = edate_gfs.replace(hour=18)
                interval_metp = to_timedelta('24H')
                sdate_metp_str = sdate_metp.strftime("%Y%m%d%H%M")
                edate_metp_str = edate_metp.strftime("%Y%m%d%H%M")
                interval_metp_str = timedelta_to_HMS(interval_metp)
            else:
                # Use same cycledef as gfs if there is no more than one per day
                sdate_metp_str = sdate_gfs_str
                edate_metp_str = edate_gfs_str
                interval_metp_str = interval_gfs_str

            strings.append(f'\t<cycledef group="metp">{sdate_metp_str} {edate_metp_str} {interval_metp_str}</cycledef>')

        strings.append('')
        strings.append('')

        return '\n'.join(strings)
