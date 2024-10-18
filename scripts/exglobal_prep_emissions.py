#!/usr/bin/env python3
"""
This script initializes a logger, reads configuration from the environment, and performs emissions pre-processing tasks using the AerosolEmissions class.

The script does the following:
1. Initializes a root logger with the specified logging level and colored log output.
2. Reads configuration from the environment and converts it into a Python dictionary.
3. Instantiates an AerosolEmissions object with the configuration.
4. Retrieves specific keys from the emissions task configuration and stores them in a dictionary.
5. Sets the 'emistype' attribute in the configuration dictionary based on the 'emistype' value in the emissions configuration.
6. Initializes, configures, runs, and finalizes the emissions task using the provided parameters.

Note: Make sure to have the necessary dependencies (wxflow, pygfs) installed to run this script successfully.
"""
import os

from wxflow import Logger, AttrDict, cast_strdict_as_dtypedict
from pygfs import AerosolEmissions


# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the emissions pre-processing task
    emissions = AerosolEmissions(config)

    # get local keys for configuration
    keys = ['DATA', 'forecast_dates', 'PDY', 'cyc', 'aero_emission_yaml']
    edict = AttrDict()
    for key in keys:
        edict[key] = emissions.task_config[key]
    edict['CONFIG'] = edict.aero_emission_yaml.aero_emissions['config']
    edict.aero_emission_yaml['emistype'] = edict['CONFIG'].emistype

    # print(aero_emission_yaml.aero_emissions['fix_data'])
    emissions.initialize(edict.CONFIG)
    emissions.configure(edict.aero_emission_yaml)
    emissions.run(workdir=edict.DATA, current_date=edict.PDY, forecast_dates=edict.forecast_dates, Config_dict=edict.CONFIG)
    emissions.finalize(edict['CONFIG'])
