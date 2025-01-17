#!/usr/bin/env python3

import os

from pygfs.task.globus_hpss import GlobusHpss
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the globus object
    globus = GlobusHpss(config)

    keys = ['STAGE_DIR', 'current_cycle', 'RUN', 'PDY', 'NMEM_ENS', 'HOMEgfs', 'sven_dir',
            'DATASETS_YAML']

    globus_dict = AttrDict()
    for key in keys:
        try:
            globus_dict[key] = globus.task_config[key]
        except KeyError:
            logger.warning(f"WARNING: key ({key}) not found in globus.task_config!")

    # Determine which tarballs to send
    transfer_set = globus.configure(globus_dict)

    # Send the tarballs to HPSS via Niagara
    globus.execute_transfer_data(transfer_set)

    # Clean up any temporary files
    globus.clean()


if __name__ == '__main__':
    main()
