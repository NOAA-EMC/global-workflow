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
    stage_variables = stage.calculate_stage_vars()

    if "OCNRES" in stage_variables:
        stage_variables["OCNRES"] = f"{stage_variables['OCNRES']:03d}"

    # Stage ICs
    stage.execute_stage(stage_variables)


if __name__ == '__main__':
    main()
