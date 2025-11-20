#!/usr/bin/env python3

import os

from pygfs.task.stage_ic import Stage
from wxflow import Logger, cast_strdict_as_dtypedict, logit

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    # Instantiate the Stage object
    stage = Stage(cast_strdict_as_dtypedict(os.environ))

    # Create staging dictionary with all necessary variables
    stage_dict = stage.create_stage_dict()

    # Get list of members to process
    member_list = stage.get_member_list(
        stage_dict.RUN,
        stage_dict.NMEM_ENS,
        m_index=stage_dict.get('m_index', 0),
        gefstype=stage.task_config.get('GEFSTYPE', None)
    )

    # Loop through members and stage ICs for each
    for member in member_list:
        logger.info(f"Staging initial conditions for member: {member}")

        # Get member-specific COM paths and merge into stage_dict
        stage_dict.update(stage.get_member_com_paths(stage_dict, member))

        # Execute staging
        stage.execute_stage(stage_dict)


if __name__ == '__main__':
    main()
