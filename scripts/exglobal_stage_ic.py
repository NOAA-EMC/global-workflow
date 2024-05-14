#!/usr/bin/env python3

import os

from pygfs.task.stage import Stage
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, chdir, logit

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)

@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Stage object
    stage = Stage(config)

    #Pull out all the configuration keys needed to run stage job
    keys = ['RUN','MODE','EXP_WARM_START','CDUMP','rCDUMP',
            'ntiles',
            'BASE_CPLIC','waveGRD','OCNRES','USE_OCN_PERTURB_FILES',
            'CPL_ATMIC','CPL_ICEIC','CPL_MEDIC','CPL_OCNIC','CPL_WAVIC']

    stage_dict = AttrDict()
    for key in keys:
        stage_dict[key] = stage.task_config[key]

    # Also import all COM* directory and template variables
    for key in stage.task_config.keys():
        if key.startswith("COM"):
            stage_dict[key] = stage.task_config[key]

    #TEST PRINT
    #for key in stage_dict:
    #    print(f'{key} = {stage_dict[key]}')

    cwd = os.getcwd()

    os.chdir(config.ROTDIR)

    # Determine which ICs to stage
    stage_set = stage.determine(stage_dict)

    # Stage ICs
    # TODO - create and invoke copies
    stage.execute(stage_set)

    os.chdir(cwd)

if __name__ == '__main__':
    main()
