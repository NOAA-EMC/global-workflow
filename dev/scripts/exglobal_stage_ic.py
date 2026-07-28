#!/usr/bin/env python3

import os
import sys

# Import com_paths from dev/workflow to get the canonical COM_*_TMPL definitions.
_workflow_dir = os.path.join(os.environ.get('HOMEglobal', ''), 'dev', 'workflow')
if _workflow_dir not in sys.path:
    sys.path.insert(0, _workflow_dir)
from com_paths import get_com_templates  # noqa: E402

from pygfs.task.stage_ic import Stage  # noqa: E402
from wxflow import Logger, cast_strdict_as_dtypedict, logit  # noqa: E402

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)
    # Set a default value for ATMINC_GRID if it is not in the environment
    # This MUST be done *before* config is passed to the Stage constructor.
    config.setdefault('ATMINC_GRID', '')

    # Instantiate the Stage object
    stage = Stage(config)

    # Create staging dictionary with all necessary variables
    stage_dict = stage.create_stage_dict()

    # Inject COM_*_TMPL templates: canonical defaults from com_paths.py
    # overridden by any matching values present in the environment.
    com_templates = get_com_templates()
    env_overrides = {k: v for k, v in os.environ.items()
                     if k.startswith('COM_') and k.endswith('_TMPL')}
    com_templates.update(env_overrides)
    stage_dict.update(com_templates)

    # Loop through members and stage ICs for each
    for member in stage_dict.member_list:
        logger.info(f"Staging initial conditions for member: {member}")

        # Get member-specific COM paths
        member_com_paths = stage.get_member_com_paths(stage_dict, member)

        # Create member-specific staging dict to avoid modifying base stage_dict
        stage_mem_dict = stage_dict.deepcopy()
        stage_mem_dict.update(member_com_paths)
        stage_mem_dict.update({'member': member})

        # Execute staging
        stage.execute_stage(stage_mem_dict)


if __name__ == '__main__':
    main()
