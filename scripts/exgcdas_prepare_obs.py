#!/usr/bin/env python3
# exgcdas_prepare_obs.py
# This script will collect and preprocess
# aerosol optical depth observations for
# global aerosol assimilation
import os

from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, parse_j2yaml
from pyobsforge.task.aero_prepobs import AerosolObsPrep

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config_env = cast_strdict_as_dtypedict(os.environ)
    # Take configuration from YAML file to augment/append config dict
    config_yaml = parse_j2yaml(os.path.join(config_env['HOMEgcafs'], 'parm', 'prepare_aod_obs.yaml'), config_env)
    # ensure we are not duplicating keys between the environment and the YAML config
    obsforge_dict = {}
    for key, value in config_yaml['obsforge'].items():
        if key not in config_env.keys():
            obsforge_dict[key] = value
    # Combine configs together
    config = AttrDict(**config_env, **obsforge_dict)
    config = AttrDict(**config, **config_yaml['aoddump'])

    aeroObs = AerosolObsPrep(config)
    aeroObs.initialize()
    aeroObs.execute()
    aeroObs.finalize()
