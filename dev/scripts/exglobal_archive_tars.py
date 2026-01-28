#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from pygfs.utils.archive_tar_vars import ArchiveTarVars
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
        except KeyError:
            logger.info(f"key ({key}) not found in archive.task_config!")

    # Collect all variables for YAML templates (config keys, COM paths, cycle vars, member paths)
    # Note: COM paths are relative to ROTDIR for portability in tar archives
    archive_dict = ArchiveTarVars.get_all_yaml_vars(archive.task_config)

    with chdir(config.ROTDIR):
        logger.debug(f"Changed working directory to {config.ROTDIR}")

        # Determine which archives to create
        atardir_sets = archive.configure_tars(archive_dict)

        # Create the backup tarballs and store in ATARDIR
        for atardir_set in atardir_sets:
            logger.debug(f"Processing archive set: {atardir_set['name']}")
            archive.execute_backup_dataset(atardir_set)

        # Clean up any temporary files
        logger.debug("Cleaning up temporary files and directories")
        archive.clean()

    logger.info(f"Returned to working directory {os.getcwd()}")


if __name__ == '__main__':
    main()
