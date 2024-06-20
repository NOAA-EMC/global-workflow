#!/usr/bin/env python3

import os

from pygfs.task.stage import Stage
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Stage object
    stage = Stage(config)

    # Pull out all the configuration keys needed to run stage job
    keys = ['RUN', 'MODE', 'CASE', 'CASE_ENS', 'OCNRES', 'ICERES', 'waveGRD',
            'EXP_WARM_START', 'current_cycle', 'CDUMP', 'rCDUMP',
            'ROTDIR', 'PARMgfs', 'ICSDIR', 'DTG_PREFIX',
            'ntiles', 'MEMDIR', 'REPLAY_ICS',
            'DO_WAVE', 'DO_OCN', 'DO_ICE', 'DO_NEST',
            'CPL_ATMIC', 'CPL_ICEIC', 'CPL_OCNIC', 'CPL_WAVIC']

    stage_dict = AttrDict()
    for key in keys:
        stage_dict[key] = stage.task_config[key]

    # Also import all COM* directory and template variables
    for key in stage.task_config.keys():
        if key.startswith("COM"):
            stage_dict[key] = stage.task_config[key]

    # Add the os.path.exists function to the dict for yaml parsing
    stage_dict['path_exists'] = os.path.exists

    # Stage ICs
    stage.execute_stage(stage_dict)


if __name__ == '__main__':
    main()
