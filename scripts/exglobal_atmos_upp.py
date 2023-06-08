#!/usr/bin/env python3

import os

from pygw.attrdict import AttrDict
from pygw.logger import Logger, logit
from pygw.configuration import cast_strdict_as_dtypedict
from pygfs.task.upp import UPP

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    upp = UPP(config)

    keys = ['HOMEgfs', 'DATA', 'current_cycle', 'RUN',
            'COM_ATMOS_ANALYSIS', 'COM_ATMOS_HISTORY', 'COM_ATMOS_MASTER',
            'upp_run',
            'APRUN_UPP',
            'forecast_hour', 'valid_datetime',
            'atmos_filename', 'flux_filename']
    upp_dict = AttrDict()
    for key in keys:
        upp_dict[key] = upp.task_config[key]

    upp_yaml = upp.task_config.upp_yaml

    upp.initialize(upp_yaml)
    upp.configure(upp_dict, upp_yaml)
    upp.execute(upp_dict.DATA, upp_dict.APRUN_UPP, upp_dict.forecast_hour)
    upp.finalize(upp_dict.upp_run, upp_yaml)


if __name__ == '__main__':
    main()
