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

    keys = ['STAGE_DIR', 'current_cycle', 'RUN', 'PDY', 'HOMEgfs', 'sven_dropbox',
            'doorman_gendel', 'DATASETS_YAML', 'PARMgfs', 'COMIN_CONF', 'KEEPDATA',
            'jobid', 'hpss_target_dir', 'server_home', 'SERVER_NAME', 'DOORMAN_ROOT',
            'CLIENT_GLOBUS_UUID', 'SERVER_GLOBUS_UUID', 'PSLOT', 'ENSGRP']

    globus_dict = AttrDict()
    for key in keys:
        try:
            globus_dict[key] = globus.task_config[key]
        except KeyError:
            logger.warning(f"WARNING: key ({key}) not found in globus.task_config!")

    # Determine which tarballs to send
    transfer_sets = globus.configure(globus_dict)

    # Send the tarballs to HPSS via Mercury.  Start with non-rstprod (standard) data
    count_sets = 0
    for transfer_set in ["standard", "rstprod"]:
        if len(transfer_sets[transfer_set]['locations']) > 0:
            has_rstprod = transfer_set == "rstprod"
            globus.execute_transfer_data(transfer_sets[transfer_set], has_rstprod)
            count_sets += 1

    if count_sets == 0:
        raise RuntimeError("FATAL ERROR: Transfer sets were all empty!")

    # Clean up any temporary files
    globus.clean()


if __name__ == '__main__':
    main()
