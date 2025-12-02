#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from pygfs.task.archive_vars import ArchiveVrfy
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit, chdir

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive object for execute_store_products
    archive = Archive(config)

    # Instantiate the ArchiveVrfy object for variable and file set calculation
    archive_vars = ArchiveVrfy(config)

    # update these keys to be 3 digits if they are part of archive.task_config.keys
    for key in ['OCNRES', 'ICERES']:
        try:
            archive.task_config[key] = f"{archive.task_config[key]:03d}"
        except KeyError as ee:
            logger.info(f"key ({key}) not found in archive.task_config!")

    # Get the RUN type and NET to determine which arcdir method to call
    RUN = archive.task_config.RUN
    NET = archive.task_config.get('NET', 'gfs')

    with chdir(config.ROTDIR):

        # Determine which system we're archiving for and call the appropriate method
        if NET == 'gefs':
            logger.info(f"Archiving GEFS data for cycle {archive.task_config.current_cycle}")
            arcdir_result = archive_vars.gefs_arcdir()
        elif NET == 'gcafs':
            logger.info(f"Archiving GCAFS data for cycle {archive.task_config.current_cycle}")
            arcdir_result = archive_vars.gcafs_arcdir()
        else:  # gfs, gdas (default)
            logger.info(f"Archiving GFS/GDAS data for RUN={RUN}, cycle {archive.task_config.current_cycle}")
            arcdir_result = archive_vars.gfs_arcdir()

        # Extract the file_set and mkdir_list from the result
        file_set = arcdir_result['file_set']
        mkdir_list = arcdir_result['mkdir_list']

        # Construct the arcdir_set in the format expected by execute_store_products
        arcdir_set = {
            'mkdir': mkdir_list,
            'copy': file_set
        }

        logger.info(f"Archiving {len(file_set)} files to {len(mkdir_list)} directories")
        logger.debug(f"arcdir_set: {arcdir_set}")

        # Populate the product archive (ARCDIR)
        archive.execute_store_products(arcdir_set)

        # Clean up any temporary files
        archive.clean()


if __name__ == '__main__':
    main()
