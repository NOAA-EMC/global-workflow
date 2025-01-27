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
    keys = ['current_cycle', 'RUN', 'PSLOT', 'ROTDIR', 'PARMgfs', 'VFYARC',
            'ARCDIR', 'MODE', 'DO_JEDIATMENS', 'DO_FIT2OBS', 'DO_JEDIATMVAR',
            'DO_JEDISNOWDA', 'DO_AERO_ANL', 'DO_PREP_OBS_AERO', 'NET', 'MODE', 'FHOUT_GFS',
            'FHMAX_HF_GFS', 'FHOUT_GFS', 'FHMAX_FITS', 'FHMAX', 'FHOUT', 'FHMAX_GFS']

    archive_dict = AttrDict()
    for key in keys:
        archive_dict[key] = archive.task_config.get(key)
        if archive_dict[key] is None:
            print(f"Warning: key ({key}) not found in task_config!")

    # Also import all COMIN* directory and template variables
    for key in archive.task_config.keys():
        if key.startswith("COM"):
            archive_dict[key] = archive.task_config[key]

    cwd = os.getcwd()

    os.chdir(config.ROTDIR)

    # Determine which archives to create
    arcdir_set = archive.configure_vrfy(archive_dict)

    # Populate the product archive (ARCDIR)
    archive.execute_store_products(arcdir_set)

    os.chdir(cwd)


if __name__ == '__main__':
    main()
