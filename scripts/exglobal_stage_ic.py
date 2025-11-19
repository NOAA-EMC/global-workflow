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

    # Determine member range
    first_mem, last_mem = stage._select_first_and_final_member(stage_dict.RUN, stage_dict.NMEM_ENS)

    # Loop through members and stage ICs for each
    for member in range(first_mem, last_mem + 1):
        logger.info(f"Staging initial conditions for member: {member}")

        # Get member-specific COM paths and merge into stage_dict
        stage_dict.update(stage.get_member_com_paths(stage_dict, member))

        # Execute staging
        stage.execute_stage(stage_dict)


if __name__ == '__main__':
    main()
