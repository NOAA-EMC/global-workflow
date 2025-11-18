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

    # Calculate member configuration
    stage.calculate_member()

    # Loop through members and stage ICs for each
    for member in range(stage.task_config.first_mem, stage.task_config.last_mem + 1):
        logger.info(f"Staging initial conditions for member: {member}")
        stage.execute_stage(stage.task_config, member=member)


if __name__ == '__main__':
    main()
