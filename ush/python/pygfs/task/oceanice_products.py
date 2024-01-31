#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, Any, Union
from pprint import pformat

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
    LATLON_DIMS_MAP = {'0p25': [1440, 721], '0p50': [720, 361], '1p00': [360,181], '5p00': [72, 36]}

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

        localdict = AttrDict(
            {'component': self.config.COMPONENT,
             'forecast_hour': self.config.FORECAST_HOUR,
             'valid_datetime': valid_datetime,
             'model_grid': model_grid,
             'product_grids': self.VALID_PRODUCT_GRIDS[model_grid]}
        )
        self.task_config = AttrDict(**self.config, **self.runtime_config, **localdict)

        # Read the oceanice_products.yaml file for common configuration
        logger.info(f"Read the ocean ice products configuration yaml file {self.config.OCEANICEPRODUCTS_CONFIG}")
        self.task_config.oceanice_yaml = parse_j2yaml(self.config.OCEANICEPRODUCTS_CONFIG, self.task_config)
        logger.debug(f"oceanice_yaml:\n{pformat(self.task_config.oceanice_yaml)}")

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the work directory by copying all the common fix data

        Parameters
        ----------
        """

        # Copy static data to run directory
        logger.info("Copy static data to run directory")
        FileHandler(self.task_config.oceanice_yaml.ocnicepost.fix_data).sync()

        # Copy "component" specific model data to run directory (e.g. ocean/ice forecast output)
        logger.info(f"Copy {self.task_config.component} data to run directory")
        FileHandler(self.task_config.oceanice_yaml[self.task_config.component].data_in).sync()

    @logit(logger)
    def configure(self, product_grid: str) -> None:
        """Configure the namelist for the product_grid in the work directory.
        Create namelist 'ocnicepost.nml' from template

        Parameters
        ----------
        product_grid : str
            Target product grid to process
        """

        # Make a localconf with the "component" specific configuration for parsing the namelist
        localconf = AttrDict()
        localconf.DATA = self.task_config.DATA
        localconf.component = self.task_config.component
        localconf.interpolation_weights_dir = self.task_config.DATA

        model_grid_dims = self.TRIPOLE_DIMS_MAP[self.task_config.model_grid]
        nlevs = 0  # ice
        if self.task_config.component in ['ocean']:
            #nlevs = self.task_config.ocean_levels  TODO: need to get this from config files
            #nlevs = 40
            nlevs = 25
        model_grid_dims.append(nlevs)

        localconf.source_tripole_dims = ', '.join(map(str, model_grid_dims))
        localconf.target_latlon_dims = ', '.join(map(str, self.LATLON_DIMS_MAP[product_grid]))

        localconf.maskvar = self.task_config.oceanice_yaml[self.task_config.component].namelist.maskvar
        localconf.sinvar = self.task_config.oceanice_yaml[self.task_config.component].namelist.sinvar
        localconf.cosvar = self.task_config.oceanice_yaml[self.task_config.component].namelist.cosvar
        localconf.angvar = self.task_config.oceanice_yaml[self.task_config.component].namelist.angvar
        localconf.debug = ".true." if self.task_config.oceanice_yaml.ocnicepost.namelist.debug else ".false."

        logger.debug(f"localconf:\n{pformat(localconf)}")

        # Configure the namelist and write to file
        logger.info("Create namelist for ocnicepost.x")
        nml_template = os.path.join(localconf.DATA, "ocnicepost.nml.jinja2")
        nml_data = Jinja(nml_template, localconf).render
        logger.debug(f"ocnicepost_nml:\n{nml_data}")
        nml_file = os.path.join(localconf.DATA, "ocnicepost.nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

    @logit(logger)
    def execute(self, product_grid: str) -> None:
        """Run the ocnicepost.x executable to interpolate and convert to grib2

        Parameters
        ----------
        product_grid : str
            Target product grid to process

        Returns
        -------
        None
        """

        # Run the ocnicepost.x executable
        self.interp(exec_name="ocnicepost.x")

        # Convert interpolated netCDF file to grib2
        self.netCDF_to_grib2(product_grid)

        # Index the grib2 product files
        self.netCDF_to_grib2(product_grid)


    @logit(logger)
    def interp(self, exec_name: str = "ocnicepost.x") -> None:
        """
        Run the interpolation executable

        Parameters
        ----------
        workdir : str | os.PathLike
            Working directory where to run containing the necessary files and executable
        aprun_cmd : str
            Launcher command e.g. mpirun -np <ntasks> or srun, etc.
        exec_name : str
            Name of the executable e.g. ocnicepost.x

        Returns
        -------
        None
        """
        os.chdir(self.task_config.DATA)

        exec_cmd = Executable(self.task_config.APRUN_OCNICEPOST)
        exec_cmd.add_default_arg(os.path.join(self.task_config.DATA, exec_name))

        self._call_executable(exec_cmd)


    @logit(logger)
    def netCDF_to_grib2(self, grid: str) -> None:

        os.chdir(self.task_config.DATA)

        exec_cmd = Executable(os.path.join(self.task_config.HOMEgfs, "ush", "oceanice_nc2grib2.sh"))
        arguments = [self.task_config.component, grid, self.task_config.valid_datetime.strftime("%Y%m%d%H"), "0-6"]
        if self.task_config.component == 'ocean':
            levs = self.task_config.oceanice_yaml.ocean.namelist.ocean_levels
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

    @logit(logger)
    def finalize(self) -> None:
        """Perform closing actions of the task.
        Copy data back from the DATA/ directory to COM/

        Parameters
        ----------
        None

        """

        # Copy "component" specific generated data to COM/ directory
        data_out = self.task_config.oceanice_yaml[self.task_config.component].data_out

        logger.info(f"Copy processed data to COM/ directory")
        FileHandler(data_out).sync()
