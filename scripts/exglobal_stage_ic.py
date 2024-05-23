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
    keys = ['RUN','MODE','DO_WAVE','DO_OCN','DO_ICE','DO_NEST',
            'current_cycle','EXP_WARM_START','CDUMP','rCDUMP',
            'ROTDIR','PARMgfs','ntiles','MEMDIR',
            'BASE_CPLIC','waveGRD','OCNRES',
            #TODO: Need this for mediator#'CPL_MEDIC',
            'CPL_ATMIC','CPL_ICEIC','CPL_OCNIC','CPL_WAVIC']

    keys_gefs = ['USE_OCN_PERTURB_FILES']

    if stage.task_config['RUN'] == "gefs":
        keys.extend(keys_gefs)

    stage_dict = AttrDict()
    for key in keys:
        stage_dict[key] = stage.task_config[key]

    # Also import all COM* directory and template variables
    for key in stage.task_config.keys():
        if key.startswith("COM"):
            stage_dict[key] = stage.task_config[key]

    #TEST PRINT
    for key in stage_dict:
        print(f'{key} = {stage_dict[key]}')

    cwd = os.getcwd()

    os.chdir(config.ROTDIR)

    # Determine which ICs to stage
    #stage_sets = stage.determine_stage(stage_dict)
    stage_set = stage.determine_stage(stage_dict)

    # Stage ICs
    #for stage_set in stage_sets:
    #   print(f'set = {stage_set}')
    #   stage.execute_stage(stage_set)
    print(f'set = {stage_set}')
    stage.execute_stage(stage_set)

    os.chdir(cwd)

if __name__ == '__main__':
    main()
