#!/usr/bin/env python3

import os

from pygw.logger import Logger, logit
from pygw.configuration import cast_strdict_as_dtypedict
from pygfs.task.upp import UPP

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    forecast_hour = config.get("FORECAST_HOUR", 0)
    upp_run = config.get("UPP_RUN", "forecast")

    upp = UPP(config)
    upp.initialize()

    upp.pre_execute(upp_run, forecast_hour)
    upp.execute()
    upp.post_execute(upp_run, forecast_hour)

    upp.finalize()


if __name__ == '__main__':
    main()
