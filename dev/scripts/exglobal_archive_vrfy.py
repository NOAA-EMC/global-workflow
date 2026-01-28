#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from pygfs.utils.archive_vrfy_vars import ArchiveVrfyVars
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit, chdir

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive task object
    archive = Archive(config)

    # Collect all archive variables in complete arch_dict for YAML templates
    # Use static utility methods from ArchiveVrfyVars
    arch_dict = ArchiveVrfyVars.get_all_yaml_vars(archive.task_config)

    # Pass arch_dict to configure_vrfy which will render the Jinja2 YAML
    arcdir_set = archive.configure_vrfy(arch_dict)

    with chdir(config.ROTDIR):

        # Populate the product archive (ARCDIR)
        archive.execute_store_products(arcdir_set)

        # Clean up any temporary files
        archive.clean()


if __name__ == '__main__':
    main()
