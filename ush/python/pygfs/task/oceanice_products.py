#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, Any
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
                    Executable, which)

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

        if self.task_config.COMPONENT not in self.VALID_COMPONENTS:
            raise NotImplementedError(f'{self.task_config.COMPONENT} is not a valid model component.\n' +
                                      'Valid model components are:\n' +
                                      f'{", ".join(self.VALID_COMPONENTS)}')

        model_grid = f"mx{self.task_config[self.COMPONENT_RES_MAP[self.task_config.COMPONENT]]:03d}"

        valid_datetime = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.FORECAST_HOUR}H"))

        forecast_hour = self.task_config.FORECAST_HOUR
        if self.task_config.COMPONENT == 'ice':
            interval = self.task_config.FHOUT_ICE_GFS
        if self.task_config.COMPONENT == 'ocean':
            interval = self.task_config.FHOUT_OCN_GFS

        # TODO: This is a bit of a hack, but it works for now
        # FIXME: find a better way to provide the averaging period
        avg_period = f"{forecast_hour - interval:03d} - {forecast_hour:03d}"

        # Extend task_config with localdict
        localdict = AttrDict(
            {'component': self.task_config.COMPONENT,
             'forecast_hour': forecast_hour,
             'valid_datetime': valid_datetime,
             'avg_period': avg_period,
             'model_grid': model_grid,
             'interval': interval,
             'product_grids': self.VALID_PRODUCT_GRIDS[model_grid]}
        )
        self.task_config = AttrDict(**self.task_config, **localdict)

        # Read the oceanice_products.yaml file for common configuration
        logger.info(f"Read the ocean ice products configuration yaml file {self.task_config.OCEANICEPRODUCTS_CONFIG}")
        self.task_config.oceanice_yaml = parse_j2yaml(self.task_config.OCEANICEPRODUCTS_CONFIG, self.task_config)
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
        localconf.write_grib2 = ".true." if config.oceanice_yaml.ocnicepost.namelist.write_grib2 else ".false."
        localconf.write_netcdf = ".true." if config.oceanice_yaml.ocnicepost.namelist.write_netcdf else ".false."

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

        # Run the ocnicepost.x executable if interpolated variables are wanted
        if config.oceanice_yaml.ocnicepost.namelist.write_netcdf or config.oceanice_yaml.ocnicepost.namelist.write_grib2:
            OceanIceProducts.interp(config.DATA, config.APRUN_OCNICEPOST, exec_name="ocnicepost.x")

        if config.oceanice_yaml.ocnicepost.namelist.write_grib2:
            # Index the interpolated grib2 file
            OceanIceProducts.index(config, product_grid)

    @staticmethod
    @logit(logger)
    def interp(workdir: str, aprun_cmd: str, exec_name: str = "ocnicepost.x") -> None:
        """
        Run the interpolation executable to generate interpolated file

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
        try:
            exec_cmd()
        except Exception:
            logger.exception(f"FATAL ERROR: Error occurred during execution of {exec_cmd}")
            raise WorkflowException(f"{exec_cmd}")

    @staticmethod
    @logit(logger)
    def index(config: Dict, grid: str) -> None:
        """
        Index the grib2 file

        Parameters
        ----------
        config : Dict
            Configuration dictionary for the task
        grid : str
            Target product grid to process

        Environment Parameters
        ----------------------
        WGRIB2: str (optional)
            path to executable "wgrib2"
            Typically set in the modulefile

        Returns
        -------
        None
        """

        os.chdir(config.DATA)
        logger.info("Generate index file")

        wgrib2_cmd = os.environ.get("WGRIB2", None)

        grbfile = f"{config.component}.{grid}.grib2"
        grbfidx = f"{grbfile}.idx"

        if not os.path.exists(grbfile):
            logger.warning(f"WARNING: No {grbfile} to index!")
            return

        logger.info(f"Creating index file for {grbfile}")
        exec_cmd = which("wgrib2") if wgrib2_cmd is None else Executable(wgrib2_cmd)
        exec_cmd.add_default_arg("-s")
        try:
            exec_cmd(grbfile, output=grbfidx)
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
        config: Dict
            Configuration dictionary for the task

        Returns
        -------
        None
        """

        os.chdir(config.DATA)

        input_file = f"{config.component}.nc"
        output_file = f"{config.component}_subset.nc"

        varlist = config.oceanice_yaml[config.component].subset.variables

        logger.info(f"Subsetting {varlist} from {input_file} to {output_file}")

        try:
            # open the netcdf file
            ds = xr.open_dataset(input_file)
            if config.component == 'ice':
                # subset the variables
                ds_subset = ds[varlist]
                # remove coords that were carried from original file but not used
                ds_subset = ds_subset.drop_vars('ELON', errors='ignore')
                ds_subset = ds_subset.drop_vars('ELAT', errors='ignore')
                ds_subset = ds_subset.drop_vars('NLON', errors='ignore')
                ds_subset = ds_subset.drop_vars('NLAT', errors='ignore')

            if config.component == 'ocean':
                # subset ocean variables for z_levels in products
                levels = config.oceanice_yaml.ocean.namelist.ocean_levels
                ds_subset = ds[varlist].sel(z_l=levels)

            # save global attributes from the old netcdf file into new netcdf file
            ds_subset.attrs = ds.attrs

            # save subsetted variables to a new netcdf file and compress
            if config.oceanice_yaml[config.component].subset.compress:
                compress_with = config.oceanice_yaml[config.component].subset.compress_with
                compress_level = config.oceanice_yaml[config.component].subset.compress_level
                default_compression = {compress_with: True, "complevel": int(compress_level)}
                compress_encoding = {var_name: default_compression for var_name in ds_subset.data_vars}
                ds_subset.to_netcdf(output_file, encoding=compress_encoding)
            else:
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
