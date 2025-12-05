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

    # Instantiate the ArchiveVrfy object to collect variables to render YAML
    archive_vars = ArchiveVrfy(config)

    with chdir(config.ROTDIR):

        # Collect all archive variables in complete arch_dict for YAML templates
        # This method handles everything: general vars, NET dispatch, cycle_vars, com_paths, file_set, mkdir_list
        arch_dict = archive_vars.get_all_yaml_vars()

        # Pass arch_dict to configure_vrfy which will render the Jinja2 YAML
        arcdir_set = archive.configure_vrfy(AttrDict(arch_dict))

        # Populate the product archive (ARCDIR)
        archive.execute_store_products(arcdir_set)


if __name__ == '__main__':
    main()
