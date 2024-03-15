#!/usr/bin/env python3

import os

from wxflow import AttrDict, Logger, logit, cast_strdict_as_dtypedict
from pygfs.task.archive import Archive

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive object
    archive = Archive(config)

    # Pull out all the configuration keys needed to run the rest of archive steps
    keys = ['ATARDIR', 'current_cycle', 'FHMIN', 'FHMAX', 'FHOUT', 'RUN', 'PDY',
        'DO_VERFRAD', 'DO_VMINMON', 'DO_VERFOZN', 'DO_ICE', 'DO_AERO', 'PARMgfs',
        'DO_OCN', 'WRITE_DOPOST', 'cyc', 'atmos_analysis_dir',
        'atmos_analysis_ensstat_dir', 'atmos_analysis_mem_dir', 'atmos_bufr_dir',
        'atmos_gempak_dir', 'atmos_grib_0p25_dir', 'atmos_grib_0p50_dir',
        'atmos_grib_1p00_dir', 'atmos_history_dir', 'atmos_history_ensstat_dir',
        'atmos_history_mem_dir', 'atmos_input_dir', 'atmos_master_dir',
        'atmos_minmon_dir', 'atmos_oznmon_dir', 'atmos_radmon_dir',
        'atmos_restart_dir', 'atmos_restart_mem_dir', 'atmos_track_dir',
        'atmos_wmo_dir', 'checm_history_dir', 'chem_analysis_dir', 'conf_dir',
        'ice_grib_dir', 'ice_history_dir', 'med_restart_dir', 'obs_dir',
        'ocean_analysis_dir', 'ocean_grib_dir', 'ocean_history_dir',
        'ocean_input_dir', 'ocean_restart_dir', 'wave_grid_dir',
        'wave_history_dir', 'wave_restart_dir', 'wave_station_dir']

    archive_dict = AttrDict()
    for key in keys:
        archive_dict[key] = archive.task_config[key]

    # Determine which archives to create
    archive_sets = archive.configure(archive_dict)

    # Create the archives
    archive.execute(archive_sets)

if __name__ == '__main__':
    main()
