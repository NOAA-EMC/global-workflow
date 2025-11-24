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
