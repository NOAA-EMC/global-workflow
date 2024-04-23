#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, chdir, logit

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive object
    archive = Archive(config)

    # Pull out all the configuration keys needed to run the rest of archive steps
    keys = ['ATARDIR', 'current_cycle', 'IAUFHRS', 'RUN', 'PDY',
            'PSLOT', 'HPSSARCH', 'LOCALARCH', 'ROTDIR', 'PARMgfs',
            'ARCDIR', 'SDATE', 'MODE', 'ENSGRP', 'NMEM_EARCGRP',
            'NMEM_ENS', 'DO_CALC_INCREMENT_ENKF_GFS', 'DO_JEDIATMENS',
            'lobsdiag_forenkf', 'FHMIN_ENKF', 'FHMAX_ENKF_GFS',
            'FHOUT_ENKF_GFS', 'FHMAX_ENKF', 'FHOUT_ENKF', 'ENKF_SPREAD',
            'restart_interval_enkfgdas', 'restart_interval_enkfgfs']

    archive_dict = AttrDict()
    for key in keys:
        archive_dict[key] = archive.task_config[key]

    # Also import all COM* directory and template variables
    for key in archive.task_config.keys():
        if key.startswith("COM"):
            archive_dict[key] = archive.task_config[key]

    cwd = os.getcwd()

    os.chdir(config.ROTDIR)

    # Determine which archives to create
    arcdir_set, atardir_sets = archive.configure(archive_dict)

    # Create the archives
    archive.execute(arcdir_set, atardir_sets)

    os.chdir(cwd)


if __name__ == '__main__':
    main()
