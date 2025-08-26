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

    # Stage ICs
    stage.calculate_stage_vars()


if __name__ == '__main__':
    main()
