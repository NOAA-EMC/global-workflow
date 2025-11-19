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

    # Set first_mem and last_mem based on RUN type
    first_mem, last_mem = stage._select_first_and_final_member(
        stage.task_config.RUN,
        stage.task_config.NMEM_ENS
    )

    # Loop through members and stage ICs for each
    for member in range(first_mem, last_mem + 1):
        logger.info(f"Staging initial conditions for member: {member}")
        
        if member >= 0:
            member_com_paths = stage.get_member_com_paths(member)
            stage.task_config.update(member_com_paths)
            stage.task_config.member = member
        
        stage.execute_stage(stage.task_config, member=member)


if __name__ == '__main__':
    main()
