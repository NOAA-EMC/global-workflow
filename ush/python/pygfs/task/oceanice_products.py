#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any
from pprint import pformat
import xarray as xr

from wxflow import (AttrDict,
                    parse_j2yaml,
                    FileHandler,
                    Jinja,
                    logit,
                    Task,
                    add_to_datetime, to_timedelta,
                    WorkflowException,
                    Executable)

logger = getLogger(__name__.split('.')[-1])


class OceanIceProducts(Task):
    """Ocean Ice Products Task
    """

    VALID_COMPONENTS = ['ocean', 'ice']
    COMPONENT_RES_MAP = {'ocean': 'OCNRES', 'ice': 'ICERES'}
    VALID_PRODUCT_GRIDS = {'mx025': ['1p00', '0p25'],
                           'mx050': ['1p00', '0p50'],
                           'mx100': ['1p00'],
                           'mx500': ['5p00']}

    # These could be read from the yaml file
    TRIPOLE_DIMS_MAP = {'mx025': [1440, 1080], 'mx050': [720, 526], 'mx100': [360, 320], 'mx500': [72, 35]}
    LATLON_DIMS_MAP = {'0p25': [1440, 721], '0p50': [720, 361], '1p00': [360, 181], '5p00': [72, 36]}

    @logit(logger, name="OceanIceProducts")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Ocean/Ice Productstask

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        if self.config.COMPONENT not in self.VALID_COMPONENTS:
            raise NotImplementedError(f'{self.config.COMPONENT} is not a valid model component.\n' +
                                      'Valid model components are:\n' +
                                      f'{", ".join(self.VALID_COMPONENTS)}')

        model_grid = f"mx{self.config[self.COMPONENT_RES_MAP[self.config.COMPONENT]]:03d}"

        valid_datetime = add_to_datetime(self.runtime_config.current_cycle, to_timedelta(f"{self.config.FORECAST_HOUR}H"))

        # TODO: This is a bit of a hack, but it works for now
        # FIXME: find a better way to provide the averaging period
        # This will be different for ocean and ice, so when they are made flexible, this will need to be addressed
        avg_period = f"{self.config.FORECAST_HOUR-self.config.FHOUT_OCNICE_GFS:03d}-{self.config.FORECAST_HOUR:03d}"

        localdict = AttrDict(
            {'component': self.config.COMPONENT,
             'forecast_hour': self.config.FORECAST_HOUR,
             'valid_datetime': valid_datetime,
             'avg_period': avg_period,
             'model_grid': model_grid,
             'product_grids': self.VALID_PRODUCT_GRIDS[model_grid]}
        )
        self.task_config = AttrDict(**self.config, **self.runtime_config, **localdict)

        # Read the oceanice_products.yaml file for common configuration
        logger.info(f"Read the ocean ice products configuration yaml file {self.config.OCEANICEPRODUCTS_CONFIG}")
        self.task_config.oceanice_yaml = parse_j2yaml(self.config.OCEANICEPRODUCTS_CONFIG, self.task_config)
        logger.debug(f"oceanice_yaml:\n{pformat(self.task_config.oceanice_yaml)}")

    @staticmethod
    @logit(logger)
    def initialize(config: Dict) -> None:
        """Initialize the work directory by copying all the common fix data

        Parameters
        ----------
        config : Dict
            Configuration dictionary for the task

        Returns
        -------
        None
        """

        # Copy static data to run directory
        logger.info("Copy static data to run directory")
        FileHandler(config.oceanice_yaml.ocnicepost.fix_data).sync()

        # Copy "component" specific model data to run directory (e.g. ocean/ice forecast output)
        logger.info(f"Copy {config.component} data to run directory")
        FileHandler(config.oceanice_yaml[config.component].data_in).sync()

    @staticmethod
    @logit(logger)
    def configure(config: Dict, product_grid: str) -> None:
        """Configure the namelist for the product_grid in the work directory.
        Create namelist 'ocnicepost.nml' from template

        Parameters
        ----------
        config : Dict
            Configuration dictionary for the task
        product_grid : str
            Target product grid to process

        Returns
        -------
        None
        """

        # Make a localconf with the "component" specific configuration for parsing the namelist
        localconf = AttrDict()
        localconf.DATA = config.DATA
        localconf.component = config.component

        localconf.source_tripole_dims = ', '.join(map(str, OceanIceProducts.TRIPOLE_DIMS_MAP[config.model_grid]))
        localconf.target_latlon_dims = ', '.join(map(str, OceanIceProducts.LATLON_DIMS_MAP[product_grid]))

        localconf.maskvar = config.oceanice_yaml[config.component].namelist.maskvar
        localconf.sinvar = config.oceanice_yaml[config.component].namelist.sinvar
        localconf.cosvar = config.oceanice_yaml[config.component].namelist.cosvar
        localconf.angvar = config.oceanice_yaml[config.component].namelist.angvar
        localconf.debug = ".true." if config.oceanice_yaml.ocnicepost.namelist.debug else ".false."

        logger.debug(f"localconf:\n{pformat(localconf)}")

        # Configure the namelist and write to file
        logger.info("Create namelist for ocnicepost.x")
        nml_template = os.path.join(localconf.DATA, "ocnicepost.nml.jinja2")
        nml_data = Jinja(nml_template, localconf).render
        logger.debug(f"ocnicepost_nml:\n{nml_data}")
        nml_file = os.path.join(localconf.DATA, "ocnicepost.nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

    @staticmethod
    @logit(logger)
    def execute(config: Dict, product_grid: str) -> None:
        """Run the ocnicepost.x executable to interpolate and convert to grib2

        Parameters
        ----------
        config : Dict
            Configuration dictionary for the task
        product_grid : str
            Target product grid to process

        Returns
        -------
        None
        """

        # Run the ocnicepost.x executable
        OceanIceProducts.interp(config.DATA, config.APRUN_OCNICEPOST, exec_name="ocnicepost.x")

        # Convert interpolated netCDF file to grib2
        OceanIceProducts.netCDF_to_grib2(config, product_grid)

    @staticmethod
    @logit(logger)
    def interp(workdir: str, aprun_cmd: str, exec_name: str = "ocnicepost.x") -> None:
        """
        Run the interpolation executable to generate rectilinear netCDF file

        Parameters
        ----------
        config : Dict
            Configuration dictionary for the task
        workdir : str
            Working directory for the task
        aprun_cmd : str
            aprun command to use
        exec_name : str
            Name of the executable e.g. ocnicepost.x

        Returns
        -------
        None
        """
        os.chdir(workdir)
        logger.debug(f"Current working directory: {os.getcwd()}")

        exec_cmd = Executable(aprun_cmd)
        exec_cmd.add_default_arg(os.path.join(workdir, exec_name))

        OceanIceProducts._call_executable(exec_cmd)

    @staticmethod
    @logit(logger)
    def netCDF_to_grib2(config: Dict, grid: str) -> None:
        """Convert interpolated netCDF file to grib2

        Parameters
        ----------
        config : Dict
            Configuration dictionary for the task
        grid : str
            Target product grid to process

        Returns
        ------
        None
        """

        os.chdir(config.DATA)

        exec_cmd = Executable(config.oceanice_yaml.nc2grib2.script)
        arguments = [config.component, grid, config.current_cycle.strftime("%Y%m%d%H"), config.avg_period]
        if config.component == 'ocean':
            levs = config.oceanice_yaml.ocean.namelist.ocean_levels
            arguments.append(':'.join(map(str, levs)))

        logger.info(f"Executing {exec_cmd} with arguments {arguments}")
        try:
            exec_cmd(*arguments)
        except OSError:
            logger.exception(f"FATAL ERROR: Failed to execute {exec_cmd}")
            raise OSError(f"{exec_cmd}")
        except Exception:
            logger.exception(f"FATAL ERROR: Error occurred during execution of {exec_cmd}")
            raise WorkflowException(f"{exec_cmd}")

    @staticmethod
    @logit(logger)
    def subset(config: Dict) -> None:
        """
        Subset a list of variables from a netcdf file and save to a new netcdf file.
        Also save global attributes and history from the old netcdf file into new netcdf file

        Parameters
        ----------
        config : Dict
            Configuration dictionary for the task

        Returns
        -------
        None
        """

        os.chdir(config.DATA)

        input_file = f"{config.component}.nc"
        output_file = f"{config.component}_subset.nc"
        varlist = config.oceanice_yaml[config.component].subset

        logger.info(f"Subsetting {varlist} from {input_file} to {output_file}")

        try:
            # open the netcdf file
            ds = xr.open_dataset(input_file)

            # subset the variables
            ds_subset = ds[varlist]

            # save global attributes from the old netcdf file into new netcdf file
            ds_subset.attrs = ds.attrs

            # save subsetted variables to a new netcdf file
            ds_subset.to_netcdf(output_file)

        except FileNotFoundError:
            logger.exception(f"FATAL ERROR: Input file not found: {input_file}")
            raise FileNotFoundError(f"File not found: {input_file}")

        except IOError as err:
            logger.exception(f"FATAL ERROR: IOError occurred during netCDF subset: {input_file}")
            raise IOError(f"An I/O error occurred: {err}")

        except Exception as err:
            logger.exception(f"FATAL ERROR: Error occurred during netCDF subset: {input_file}")
            raise WorkflowException(f"{err}")

        finally:
            # close the netcdf files
            ds.close()
            ds_subset.close()

    @staticmethod
    @logit(logger)
    def _call_executable(exec_cmd: Executable) -> None:
        """Internal method to call executable

        Parameters
        ----------
        exec_cmd : Executable
            Executable to run

        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except OSError:
            logger.exception(f"FATAL ERROR: Failed to execute {exec_cmd}")
            raise OSError(f"{exec_cmd}")
        except Exception:
            logger.exception(f"FATAL ERROR: Error occurred during execution of {exec_cmd}")
            raise WorkflowException(f"{exec_cmd}")

    @staticmethod
    @logit(logger)
    def finalize(config: Dict) -> None:
        """Perform closing actions of the task.
        Copy data back from the DATA/ directory to COM/

        Parameters
        ----------
        config: Dict
            Configuration dictionary for the task

        Returns
        -------
        None
        """

        # Copy "component" specific generated data to COM/ directory
        data_out = config.oceanice_yaml[config.component].data_out

        logger.info(f"Copy processed data to COM/ directory")
        FileHandler(data_out).sync()
