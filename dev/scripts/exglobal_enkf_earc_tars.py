#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from pygfs.utils.archive_tar_vars import ArchiveTarVars
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, chdir, logit

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive object
    archive = Archive(config)

    # Collect all variables for YAML templates (config keys, COM paths, cycle vars, member paths)
    # Note: COM paths are relative to ROTDIR for portability in tar archives
    archive_dict = ArchiveTarVars.get_all_yaml_vars(archive.task_config)

    # Change to ROTDIR for the entire archiving workflow so that relative paths
    # in YAML templates (e.g., logs/..., enkfgdas.20211221/...) are resolved
    # correctly during both file existence checks (glob) and tar creation
    with chdir(config.ROTDIR):

        # Determine which archives to create
        atardir_sets = archive.configure_tars(archive_dict)

        # Create the backup tarballs and store in ATARDIR
        for atardir_set in atardir_sets:
            archive.execute_backup_dataset(atardir_set)

        # Clean up any temporary files
        archive.clean()


if __name__ == '__main__':
    main()
