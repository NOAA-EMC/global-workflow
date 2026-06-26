#!/usr/bin/env python3

import os
import sys

from pygfs.task.archive import Archive
from pygfs.utils.archive_vars import ArchiveVrfyVars
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit, chdir

# Add dev/workflow to path for com_paths import
sys.path.insert(0, os.path.join(os.environ.get('HOMEglobal', '.'), 'dev/workflow'))
from com_paths import get_com_templates

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

    # Import all COM_*_TMPL from com_paths.py to ensure Jinja2 templates have access
    com_templates = get_com_templates()
    for key, value in com_templates.items():
        arch_dict[key] = value

    # Pass arch_dict to configure_vrfy which will render the Jinja2 YAML
    arcdir_set = archive.configure_vrfy(arch_dict)

    with chdir(config.ROTDIR):

        # Populate the product archive (ARCDIR)
        archive.execute_store_products(arcdir_set)

        # Clean up any temporary files
        archive.clean()


if __name__ == '__main__':
    main()
