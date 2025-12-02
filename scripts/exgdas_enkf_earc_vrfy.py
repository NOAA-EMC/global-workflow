#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from pygfs.task.archive_vars import ArchiveVrfy
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, chdir, logit

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive object for execute_store_products
    archive = Archive(config)

    # Instantiate the ArchiveVrfy object for variable and file set calculation
    archive_vars = ArchiveVrfy(config)

    # Get the NET and RUN type to determine which arcdir method to call
    NET = archive.task_config.get('NET', 'gfs')
    RUN = archive.task_config.RUN

    with chdir(config.ROTDIR):

        # Determine which system we're archiving for and call the appropriate method
        # EnKF runs use the GFS archiving logic with ensemble-specific handling
        logger.info(f"Archiving EnKF data for RUN={RUN}, cycle {archive.task_config.current_cycle}")

        if NET == 'gefs':
            arcdir_result = archive_vars.gefs_arcdir()
        elif NET == 'gcafs':
            arcdir_result = archive_vars.gcafs_arcdir()
        else:  # gfs, gdas, enkfgdas, enkfgfs
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

        # Populate the product archive (ARCDIR)
        archive.execute_store_products(arcdir_set)


if __name__ == '__main__':
    main()
