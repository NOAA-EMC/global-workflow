#!/usr/bin/env python3

import os

from pygfs.task.stage_ic import Stage
from wxflow import Logger, cast_strdict_as_dtypedict, logit

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Stage object
    stage = Stage(config)

    if "OCNRES" in stage.task_config:
        stage.task_config["OCNRES"] = f"{stage.task_config['OCNRES']:03d}"

    # Stage ICs
    stage.execute_stage_all_members(stage.task_config)


if __name__ == '__main__':
    main()
