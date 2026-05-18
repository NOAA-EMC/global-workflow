#!/usr/bin/env python3
# exglobal_marine_obs_dump.py
# This script will collect and preprocess
# the ocean and seaice observations for
# global marine assimilation
import os

from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, parse_j2yaml
from pygfs.task.marine_prepobs import MarineObsPrep

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config_env = cast_strdict_as_dtypedict(os.environ)

    # Take configuration from YAML file to augment/append config dict
    config_yaml = parse_j2yaml(config_env['marine_obs_provider_dict'], config_env)

    # Combine configs together
    config = AttrDict(**config_env, **obsforge_dict)
    config = AttrDict(**config, **config_yaml)

    marineObs = MarineObsPrep(config)
    marineObs.initialize()
    marineObs.execute()
    marineObs.finalize()
