#!/usr/bin/env python3

import os

from wxflow import AttrDict, Logger, logit, cast_strdict_as_dtypedict, Executable
from pygfs.task.oceanice_products import OceanIceProducts

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the OceanIce object
    oceanice = OceanIceProducts(config)

    # Pull out all the configuration keys needed to run the rest of steps
    keys = ['HOMEglobal', 'DATA', 'current_cycle', 'RUN', 'NET',
            'job', 'cycle',
            f'COMIN_{oceanice.task_config.component.upper()}_HISTORY',
            f'COMOUT_{oceanice.task_config.component.upper()}_GRIB',
            f'COMOUT_{oceanice.task_config.component.upper()}_NETCDF',
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

    # DBN alerts for output going to NOMADS
    if os.environ.get('SENDDBN').upper() == 'YES':
        logger.debug("Sending DBN alerts")
        component = oceanice_dict.component
        if component == 'ocean':
            comout = f'{oceanice_dict.COMOUT_OCEAN_NETCDF}'
        elif component == 'ice':
            comout = f'{oceanice_dict.COMOUT_ICE_NETCDF}'
        fhour = f'{oceanice_dict.forecast_hour}'.zfill(3)
        file = os.path.join(comout, 'native', f'{oceanice_dict.RUN}.{oceanice_dict.cycle}.native.f{fhour}.nc')
        alert_type = f'{oceanice_dict.RUN}_{component}_NA_NETCDF'.upper()
        if os.path.exists(file):
            dbnroot = os.environ.get('DBNROOT')
            if dbnroot is None:
                raise KeyError("DBNROOT is not defined! Cannot call dbn_alert!!")
            dbnalert = Executable(os.path.join(os.environ.get('DBNROOT'), 'bin', 'dbn_alert'))
            dbnalert('MODEL', alert_type, '{job}', file, output=str, err=str)
        else:
            raise FileNotFoundError(f"{file} does not exit! Cannot send DBN alert!")


if __name__ == '__main__':
    main()
