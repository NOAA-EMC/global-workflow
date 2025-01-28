#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit, chdir

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive object
    archive = Archive(config)

    # update these keys to be 3 digits if they are part of archive.task_config.keys
    for key in ['OCNRES', 'ICERES']:
        try:
            archive.task_config[key] = f"{archive.task_config[key]:03d}"
        except KeyError as ee:
            logger.info(f"key ({key}) not found in archive.task_config!")

    # Pull out all the configuration keys needed to run the rest of archive steps
    keys = ['current_cycle', 'RUN', 'PSLOT', 'ROTDIR', 'PARMgfs', 'VFYARC', 'REPLAY_ICS',
            'ARCDIR', 'MODE', 'DO_JEDIATMENS', 'DO_FIT2OBS', 'DO_JEDIATMVAR', 'FHMIN_GFS',
            'DO_JEDISNOWDA', 'DO_AERO_ANL', 'DO_PREP_OBS_AERO', 'NET', 'MODE', 'FHOUT_GFS',
            'FHMAX_HF_GFS', 'FHOUT_GFS', 'FHMAX_FITS', 'FHMAX', 'FHOUT', 'FHMAX_GFS']

    archive_dict = AttrDict()
    for key in keys:
        try:
            archive_dict[key] = archive.task_config[key]
        except KeyError as ee:
            logger.warning(f"WARNING: key ({key}) not found in archive.task_config!")

    # Also import all COMIN* and COMOUT* directory and template variables
    for key in archive.task_config.keys():
        if key.startswith(("COM_", "COMIN_", "COMOUT_")):
            archive_dict[key] = archive.task_config.get(key)

    with chdir(config.ROTDIR):

        # Determine which archives to create
        arcdir_set = archive.configure_vrfy(archive_dict)

        # Populate the product archive (ARCDIR)
        archive.execute_store_products(arcdir_set)

        # Clean up any temporary files
        archive.clean()


if __name__ == '__main__':
    main()
