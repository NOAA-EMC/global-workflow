#!/usr/bin/env python3

import os

from wxflow import AttrDict, Logger, logit, cast_strdict_as_dtypedict
from pygfs.task.oceanice_products import OceanIceProducts

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the OceanIce object
    oceanice = OceanIceProducts(config)

    # Pull out all the configuration keys needed to run the rest of steps
    #keys = ['HOMEgfs', 'DATA', 'current_cycle', 'RUN', 'NET',
    #        'COM_OCEAN_HISTORY', 'COM_OCEAN_GRIB',
    #        'COM_ICE_HISTORY', 'COM_ICE_GRIB',
    #        'component', 'forecast_hour', 'valid_datetime',
    #        'src_grid', 'grids']
    #oceanice_dict = AttrDict()
    #for key in keys:
    #    oceanice_dict[key] = oceanice.task_config[key]

    # Initialize the DATA/ directory; copy static data
    oceanice.initialize()

    for grid in oceanice.task_config.product_grids:

        logger.info(f"Processing {grid} grid")

        # Configure DATA/ directory for execution; prepare namelist etc.
        oceanice.configure(grid)

        # Run the oceanice post executable to interpolate and create grib2 files
        oceanice.execute(grid)

    # Copy processed output from execute
    oceanice.finalize()


if __name__ == '__main__':
    main()
