#!/usr/bin/env python3

import os

from pygfs.task.stage_ic import Stage
from pygfs.task.stage_archive import StageArchiveUtils
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Stage object
    stage = Stage(config)
    stage_archive = StageArchiveUtils(config)

    # Pull out all the configuration keys needed to run stage job
    keys = ['RUN', 'MODE', 'EXP_WARM_START', 'NMEM_ENS',
            'assim_freq', 'current_cycle', 'previous_cycle',
            'ROTDIR', 'ICSDIR', 'STAGE_IC_YAML_TMPL', 'DO_JEDIATMVAR',
            'OCNRES', 'waveGRD', 'ntiles', 'DOIAU',
            'DO_JEDIOCNVAR', 'DO_STARTMEM_FROM_JEDIICE',
            'REPLAY_ICS', 'DO_WAVE', 'DO_OCN', 'DO_ICE', 'DO_NEST', 'DO_CA', 'DO_AERO_ANL',
            'USE_ATM_ENS_PERTURB_FILES', 'USE_OCN_ENS_PERTURB_FILES']

    stage_dict = AttrDict()
    for key in keys:
        # Make sure OCNRES is three digits
        if key == "OCNRES":
            stage.task_config.OCNRES = f"{stage.task_config.OCNRES :03d}"
        stage_dict[key] = stage.task_config[key]

    # Also import all COM* directory and template variables
    for key in stage.task_config.keys():
        if key.startswith("COM"):
            stage_dict[key] = stage.task_config[key]
        if "ENSMEM" in stage.task_config:
            stage_dict["ENSMEM"] = stage.task_config["ENSMEM"]

    # Calculate cycle variables and add to stage_dict
    cycle_variables = stage_archive.calculate_cycle_variables()
    stage_dict.update(cycle_variables)
    member_variables = stage_archive.calculate_member_variables()
    stage_dict.update(member_variables)

    # Stage ICs
    stage.execute_stage(stage_dict)


if __name__ == '__main__':
    main()
