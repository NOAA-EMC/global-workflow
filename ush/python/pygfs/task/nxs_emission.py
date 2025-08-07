#!/usr/bin/env python3

import os
import re
import xarray as xr
import subprocess
import cftime
from logging import getLogger
from typing import Dict, Any, Union, List
from dateutil.rrule import DAILY, HOURLY, rrule
from pprint import pprint
from jinja2 import Environment, FileSystemLoader
from wxflow import (AttrDict,
                    FileHandler,
                    parse_j2yaml,
                    logit,
                    Task,
                    to_timedelta,
                    WorkflowException,
                    Executable, which)

logger = getLogger(__name__.split('.')[-1])


class NXSEmissions(Task):
    """NEXUS Emissions pre-processing Task
    """

    @logit(logger, name="NXSEmissions")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the NEXUS Emissions task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        self.task_config = AttrDict(config)
        self.AERO_INPUTS_DIR = self.task_config.get('AERO_INPUTS_DIR', None)
        self.COMOUT_CHEM_INPUT = self.task_config.get('COMOUT_CHEM_INPUT', None)
        nforecast_hours = self.task_config["FHMAX_GFS"]
        self.start_date = self.task_config["SDATE"] - to_timedelta('12H')
        self.end_date = self.task_config["EDATE"] + to_timedelta('12H')
        frequency = self.task_config.get("NXS_DIAG_FREQ", "Hourly")
        if frequency == "Hourly":
            self.forecast_dates = list(rrule(freq=HOURLY, dtstart=self.start_date, until=self.end_date))
        elif frequency == 'Daily':
            self.forecast_dates = list(rrule(freq=DAILY, dtstart=self.start_date, until=self.end_date))
        else:
            raise WorkflowException(f"Unsupported NXS_DIAG_FREQ: {frequency}")

        self.forecast_dates_daily = list(rrule(freq=DAILY, dtstart=self.start_date, until=self.end_date))

        logger.info(f"NXSEmissions initialized with start date: {self.start_date}, end date: {self.end_date}")

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the work directory and process chemical emissions configuration.

        This method performs the following steps:
        1. Render the NEXUS configuration files using Jinja2 templates
           found in `parm/chem/nexus/$NXS_CONFIG`
        2. Sets up template variables for emission configuration
        3. Creates necessary working directories
        4. Copies required input files to working directory
        5. Sets up forecast dates and file paths for each date

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        WorkflowException
            If the YAML template file is not found
            If required directories cannot be created
            If file copying operations fail

        Notes
        -----
        The method expects the following configuration to be available:
        - HOMEgfs : str
            Base directory containing workflow configuration
        - DATA : str
            Working directory path
        - COMOUT_CHEM_INPUT : str
            Output directory for chemical input files
        - AERO_EMIS_FIRE_DIR : str
            Directory containing fire emission data

        The configuration is processed through a Jinja2 template system
        and the resulting setup is stored in self.task_config.
        """
        logger.info("Initializing NEXUS emissions pre-processing task")

        #
        logger.info("Rendering NEXUS configuration files")
        # Check for required NEXUS configuration parameters
        nxs_config_set = self.task_config.get('NXS_CONFIG', None)
        if not nxs_config_set:
            raise WorkflowException("NXS_CONFIG must be set in task configuration")
        nxs_config_dir = self.task_config.get('NXS_CONFIG_DIR', None)
        if not nxs_config_dir:
            raise WorkflowException("NXS_CONFIG_DIR must be set in task configuration")
        nxs_input_dir = self.task_config.get('NXS_INPUT_DIR', None)
        if not nxs_input_dir:
            raise WorkflowException("NXS_INPUT_DIR must be set in task configuration")
        # Default NXS_TSTEP to 3600 seconds (1 hour) if not set
        nxs_tstep = self.task_config.get('NXS_TSTEP', 3600)
        if not nxs_tstep:
            raise WorkflowException("NXS_TSTEP must be set in task configuration")

        logger.info(f"Using NXS_CONFIG: {nxs_config_set}")
        logger.info(f"Using NXS_CONFIG_DIR: {nxs_config_dir}")
        logger.info(f"Using NXS_INPUT_DIR: {nxs_input_dir}")
        logger.info(f"Using NXS_TSTEP: {nxs_tstep}")

        # Check for grid parameters
        if not self.task_config.get('NXS_NX', None):
            raise WorkflowException("NXS_NX must be set in task configuration")
        if not self.task_config.get('NXS_NY', None):
            raise WorkflowException("NXS_NY must be set in task configuration")
        if not self.task_config.get('NXS_NZ', None):
            raise WorkflowException("NXS_NZ must be set in task configuration")
        if not self.task_config.get('NXS_XMIN', None):
            raise WorkflowException("NXS_XMIN must be set in task configuration")
        if not self.task_config.get('NXS_XMAX', None):
            raise WorkflowException("NXS_XMAX must be set in task configuration")
        if not self.task_config.get('NXS_YMIN', None):
            raise WorkflowException("NXS_YMIN must be set in task configuration")

        logger.info(f"Grid parameters: NXS_NX={self.task_config.NXS_NX}")
        logger.info(f"Grid parameters: NXS_NY={self.task_config.NXS_NY}")
        logger.info(f"Grid parameters: NXS_NZ={self.task_config.NXS_NZ}")
        logger.info(f"Grid parameters: NXS_XMIN={self.task_config.NXS_XMIN}")
        logger.info(f"Grid parameters: NXS_XMAX={self.task_config.NXS_XMAX}")
        logger.info(f"Grid parameters: NXS_YMIN={self.task_config.NXS_YMIN}")
        logger.info(f"Grid parameters: NXS_YMAX={self.task_config.NXS_YMAX}")

        processed_nxs_files = []
        final_output_files = []
        sorted_dates = sorted(self.forecast_dates)
        for d in sorted_dates[:-1]:
            fname = f"{self.task_config.NXS_DIAG_PREFIX}.{d.strftime('%Y%m%d%H')}00.nc"
            fname_final = f"{self.task_config.NXS_DIAG_PREFIX}.{d.strftime('%Y%m%d')}.nc"
            processed_nxs_files.append(fname)
            final_output_files.append(fname_final)
        self.processed_nxs_files = processed_nxs_files
        # render the NEXUS configuration files
        if not os.path.exists(nxs_config_dir):
            raise WorkflowException(f"NEXUS configuration file not found: {nxs_config_dir}")
        logger.info(f"Rendering NEXUS configuration from {nxs_config_dir}")
        tmpl_dict = {
            'NXS_CONFIG': nxs_config_set,
            'NXS_CONFIG_DIR': nxs_config_dir,
            'NXS_INPUT_DIR': nxs_input_dir,
            'NXS_DIAG_PREFIX': self.task_config.NXS_DIAG_PREFIX,
            'NXS_TSTEP': nxs_tstep,
            'NXS_NX': self.task_config.NXS_NX,
            'NXS_NY': self.task_config.NXS_NY,
            'NXS_NZ': self.task_config.NXS_NZ,
            'NXS_XMIN': self.task_config.NXS_XMIN,
            'NXS_XMAX': self.task_config.NXS_XMAX,
            'NXS_YMIN': self.task_config.NXS_YMIN,
            'NXS_YMAX': self.task_config.NXS_YMAX,
            'LOCAL_INPUT_DIR': os.path.join(self.task_config.DATA, 'INPUT'),
            'NXS_EXECUTABLE': os.path.join(self.task_config.get('HOMEgfs', None), "exec/nexus.x"),
            "WORK_DIR": self.task_config.DATA,
            "NXS_DO_MEGAN": self.task_config.get('NXS_DO_MEGAN', False),
            "NXS_DO_CEDS2019": self.task_config.get('NXS_DO_CEDS2019', True),
            "NXS_DO_CEDS2024": self.task_config.get('NXS_DO_CEDS2024', False),
            "NXS_DO_HTAPv2": self.task_config.get('NXS_DO_HTAPv2', True),
            "NXS_DO_HTAPv3": self.task_config.get('NXS_DO_HTAPv3', False),
            "NXS_DO_CAMS": self.task_config.get('NXS_DO_CAMS', False),
            "NXS_DO_CAMSTEMPO": self.task_config.get('NXS_DO_CAMSTEMPO', False),
            "start_date": self.start_date.strftime('%Y-%m-%d %H:%M:%S'),
            "end_date": self.end_date.strftime('%Y-%m-%d %H:%M:%S'),
            "FINAL_OUTPUT": final_output_files,
            "COMOUT_CHEM_INPUT": self.task_config.COMOUT_CHEM_INPUT,
            "COMOUT_CHEM_RESTART": self.task_config.COMOUT_CHEM_RESTART,
            "RestartFile": f"HEMCO_restart.{self.end_date.strftime('%Y%m%d%H')}00.nc",
            "processed_nxs_files": processed_nxs_files,

        }

        yaml_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'chem', 'nxs_emission.yaml.j2')
        if not os.path.exists(yaml_template):
            logger.warning(f"Template file not found: {yaml_template}, using default configuration")
            yaml_config = {'nxs_emission': {}}
        else:
            logger.debug(f'Parsing YAML template: {yaml_template}')
            yaml_config = parse_j2yaml(yaml_template, tmpl_dict)

        # Add yaml configuration to task_config
        self.task_config = AttrDict(**self.task_config, **yaml_config)

        # Link NEXUS input directory to the working directory
        FileHandler(self.task_config.nxs_emission.data_in).sync()
        logger.info(f"NEXUS input directory linked to {self.task_config.DATA}")

        # Render NXS Grid File
        file_loader = FileSystemLoader(self.task_config.NXS_CONFIG_DIR)
        env = Environment(loader=file_loader)
        nxs_grid_template = env.get_template(f"{self.task_config.NXS_GRID_NAME}.j2")
        self.task_config.NXS_GRID_TEMPLATE = nxs_grid_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NXS_GRID_NAME)
        _write_txt_file(self.task_config.NXS_GRID_TEMPLATE, outfile)
        logger.info(f"NEXUS grid file rendered successfully: written to {outfile}")

        # Render NXS Config File
        nxs_config_template = env.get_template(f"{self.task_config.NXS_CONFIG_NAME}.j2")
        self.task_config.NXS_CONFIG_TEMPLATE = nxs_config_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NXS_CONFIG_NAME)
        _write_txt_file(self.task_config.NXS_CONFIG_TEMPLATE, outfile)
        logger.info(f"NEXUS config file rendered successfully: written to {outfile}")

        # Render NXS Time File
        nxs_time_template = env.get_template(f"{self.task_config.NXS_TIME_NAME}.j2")
        self.task_config.NXS_TIME_TEMPLATE = nxs_time_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NXS_TIME_NAME)
        _write_txt_file(self.task_config.NXS_TIME_TEMPLATE, outfile)
        logger.info(f"NEXUS time file rendered successfully: written to {outfile}")

        # Render NXS Diag File
        nxs_diag_template = env.get_template(f"{self.task_config.NXS_DIAG_NAME}.j2")
        self.task_config.NXS_DIAG_TEMPLATE = nxs_diag_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NXS_DIAG_NAME)
        _write_txt_file(self.task_config.NXS_DIAG_TEMPLATE, outfile)
        logger.info(f"NEXUS diag file rendered successfully: written to {outfile}")

        # Render NXS Spec File
        nxs_spec_template = env.get_template(f"{self.task_config.NXS_SPEC_NAME}.j2")
        self.task_config.NXS_SPEC_TEMPLATE = nxs_spec_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NXS_SPEC_NAME)
        _write_txt_file(self.task_config.NXS_SPEC_TEMPLATE, outfile)
        logger.info(f"NEXUS spec file rendered successfully: written to {outfile}")

    @logit(logger)
    def execute(self) -> None:
        """Run NEXUS emission preprocessor based on configuration.

        This will run the NEXUS preprocessor executable with the provided configuration.
        It will process the emission files based on the task configuration and forecast dates.
        It will also handle different types of emissions based on the configuration.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Notes
        -----
        - This method assumes that the NEXUS preprocessor executable is available in the PATH.
        - It will log the processing steps and any issues encountered.
        Raises
        ------
        WorkflowException
            If the NEXUS preprocessor executable is not found
            If the working directory does not exist
            If no emission files are found for processing
        """
        logger.info(f"Running NEXUS emission preprocessor in {self.task_config.DATA}")
        logger.info(f"NEXUS Logs: {self.task_config.DATA}/stdout")
        logger.info(f"NEXUS Logs: {self.task_config.DATA}/stderr")
        logger.info(f"NEXUS Logs: {self.task_config.DATA}/NEXUS.log")

        if not os.path.exists(self.task_config.DATA):
            raise WorkflowException(f"Working directory does not exist: {self.task_config.DATA}")

        # pprint(self.task_config)
        exe = Executable(self.task_config.launcher)
        arg_list = ['--ntasks',
                    str(1),
                    'nexus.x',
                    '-c',
                    self.task_config.NXS_CONFIG_NAME]
        exe(*arg_list, output='stdout', error='stderr')

        logger.info("Concatenating processed NEXUS files...")

        files = sorted(self.processed_nxs_files)
        dsets = []
        for f in files:
            dsets.append(xr.open_dataset(f, decode_cf=False))

        # Concatenate along time dimension
        ds = xr.concat(dsets, dim="time")

        # Convert raw time values to datetime objects using cftime
        if 'time' not in ds.dims:
            raise WorkflowException("No 'time' dimension found in NEXUS output dataset.")

        time_var = ds['time']
        time_units = time_var.attrs.get('units', None)
        time_calendar = time_var.attrs.get('calendar', 'standard')
        if time_units is None:
            raise WorkflowException("No 'units' attribute found for time variable.")

        # Convert time values to datetime objects
        time_vals = time_var.values
        time_dt = cftime.num2date(time_vals, units=time_units, calendar=time_calendar)

        # Group indices by day
        from collections import defaultdict
        day_to_indices = defaultdict(list)
        for idx, dt in enumerate(time_dt):
            day_to_indices[dt.strftime('%Y%m%d')].append(idx)

        encoding = {var: {"zlib": True, "complevel": 4} for var in ds.data_vars}
        for day_str, indices in day_to_indices.items():
            daily_ds = ds.isel(time=indices)
            outname = f"{self.task_config.NXS_DIAG_PREFIX}.{day_str}.nc"
            daily_ds.to_netcdf(outname, format="NETCDF4", encoding=encoding)
            logger.info(f"Wrote daily output: {outname}")

        logger.info("NEXUS emission processing execute phase complete")

    @logit(logger)
    def finalize(self) -> None:
        """Perform closing actions of the task.
        Copy processed files from the DATA directory to COMOUT_CHEM_INPUT.

        Returns
        -------
        None

        Notes
        -----
        Only copies processed NEXUS files to the output directory.
        Uses FileHandler for reliable file operations with logging
        """
        logger.info("Finalizing NEXUS emissions processing")

        FileHandler(self.task_config.nxs_emission.data_out).sync()

        logger.info("Chemical emissions finalization complete")


def _write_txt_file(content: str, file_path: Union[str, os.PathLike]) -> None:
    """Write content to a text file.

    Parameters
    ----------
    content : str
        Content to write to the file.
    file_path : Union[str, os.PathLike]
        Path where the file will be created.

    Returns
    -------
    None

    Notes
    -----
    If the directory does not exist, it will be created.
    """
    os.makedirs(os.path.dirname(file_path), exist_ok=True)
    with open(file_path, 'w') as f:
        f.write(content)
