#!/usr/bin/env python3

import os

from wxflow import AttrDict, Logger, logit, cast_strdict_as_dtypedict
from pygfs.task.upp import UPP

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the UPP object
    upp = UPP(config)

    # Pull out all the configuration keys needed to run the rest of UPP steps
    keys = ['HOMEgfs', 'DATA', 'current_cycle', 'RUN', 'NET',
            'COMIN_ATMOS_ANALYSIS', 'COMIN_ATMOS_HISTORY', 'COMOUT_ATMOS_MASTER',
            'upp_run',
            'APRUN_UPP',
            'forecast_hour', 'valid_datetime',
            'atmos_filename', 'flux_filename']
    upp_dict = AttrDict()
    for key in keys:
        upp_dict[key] = upp.task_config[key]

    # Get the fully parse upp.yaml file for the current cycle
    upp_yaml = upp.task_config.upp_yaml

    # Initialize the DATA/ directory; copy static data
    upp.initialize(upp_yaml)

    # Configure DATA/ directory for execution; prepare namelist etc.
    upp.configure(upp_dict, upp_yaml)

    # Run the UPP and index the master grib2 files
    upp.execute(upp_dict.DATA, upp_dict.APRUN_UPP, upp_dict.forecast_hour)

    # Copy processed output from execute
    upp.finalize(upp_dict.upp_run, upp_yaml)


if __name__ == '__main__':
    main()
