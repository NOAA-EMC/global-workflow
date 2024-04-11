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
        'DO_OCN', 'DO_WAVE', 'WRITE_DOPOST', 'cyc', 'cycle_YYYYMMDDHH',
        'PSLOT', 'cycle_HH', 'first_cycle', 'NFHRS_PER_GROUP', 'HPSSARCH',
        'DO_JEDISNOWDA', 'LOCALARCH', 'REALTIME', 'ROTDIR', 'ARCH_WARMICFREQ',
        'ARCH_FCSTICFREQ', 'SAVEFCSTIC', 'ARCH_CYC', 'assim_freq', 'ARCDIR',
        'path_exists']

    archive_dict = AttrDict()
    for key in keys:
        archive_dict[key] = archive.task_config[key]

    # Also get all relative paths
    for key in archive.task_config.keys():
        if key.endswith("_dir"):
            archive_dict[key] = archive.task_config[key]

    # Determine which archives to create
    arcdir_set, tarball_sets = archive.configure(archive_dict)

    # Create the archives
    archive.execute(arcdir_set, atardir_sets)

if __name__ == '__main__':
    main()
