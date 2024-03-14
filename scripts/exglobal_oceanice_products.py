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
    keys = ['HOMEgfs', 'DATA', 'current_cycle', 'RUN', 'NET',
            f'COM_{oceanice.task_config.component.upper()}_HISTORY', f'COM_{oceanice.task_config.component.upper()}_GRIB'
            'APRUN_OCNICEPOST',
            'component', 'forecast_hour', 'valid_datetime', 'avg_period',
            'model_grid', 'product_grids', 'oceanice_yaml']
    oceanice_dict = AttrDict()
    for key in keys:
        oceanice_dict[key] = oceanice.task_config[key]

    # Initialize the DATA/ directory; copy static data
    oceanice.initialize(oceanice_dict)

    for grid in oceanice_dict.product_grids:

        logger.info(f"Processing {grid} grid")

        # Configure DATA/ directory for execution; prepare namelist etc.
        oceanice.configure(oceanice_dict, grid)

        # Run the oceanice post executable to interpolate and create grib2 files
        oceanice.execute(oceanice_dict, grid)

    # Subset raw model data to create netCDF products
    oceanice.subset(oceanice_dict)

    # Copy processed output from execute and subset
    oceanice.finalize(oceanice_dict)


if __name__ == '__main__':
    main()
