#!/usr/bin/env python3

import os
import re
from collections import defaultdict
import xarray as xr
import subprocess
import numpy as np
import cftime
from logging import getLogger
from typing import Dict, Any, Union, List
from dateutil.rrule import DAILY, HOURLY, rrule
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


class NEXUSEmissions(Task):
    """NEXUS Emissions pre-processing Task
    """

    @logit(logger, name="NEXUSEmissions")
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

        # get the nforecast hours - gcdas will use FHMAX and gcafs will use FHMAX_GFS
        if 'das' in self.task_config['RUN']:
            nforecast_hours = self.task_config["FHMAX"]
        else:
            nforecast_hours = self.task_config["FHMAX_GFS"]

        # Create start date based on SDATE
        self.start_date = self.task_config["CDATE"]
        self.total_hrs = nforecast_hours + 3
        self.end_date = self.task_config["CDATE"] + to_timedelta(f'{self.total_hrs}H')

        logger.info(f'SDATE_GFS: {self.start_date}')
        logger.info(f'nforecast_hours: {nforecast_hours}')
        logger.info(f'Computed end_date: {self.end_date} (total_hrs={self.total_hrs})')

        # Create the forecast dates based on start_date and end_date
        frequency = self.task_config.get("NEXUS_DIAG_FREQ", "Hourly")
        if frequency == "Hourly":
            self.forecast_dates = list(rrule(freq=HOURLY, dtstart=self.start_date, until=self.end_date))
        elif frequency == 'Daily':
            self.forecast_dates = list(rrule(freq=DAILY, dtstart=self.start_date, until=self.end_date))
        else:
            raise WorkflowException(f"Unsupported NEXUS_DIAG_FREQ: {frequency}")

        self.forecast_dates_daily = list(rrule(freq=DAILY, dtstart=self.start_date, until=self.end_date))

        logger.info(f"NEXUSEmissions initialized with start date: {self.start_date}, end date: {self.end_date}")

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the work directory and process chemical emissions configuration.

        This method performs the following steps:
        1. Render the NEXUS configuration files using Jinja2 templates
           found in `parm/chem/nexus/$NEXUS_CONFIG`
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
        required_nexus_params = [
            'NEXUS_CONFIG',
            'NEXUS_CONFIG_DIR',
            'NEXUS_INPUT_DIR',
        ]
        for param in required_nexus_params:
            if not self.task_config.get(param, None):
                raise WorkflowException(f"{param} must be set in task configuration")

        nexus_config_set = self.task_config.get('NEXUS_CONFIG', None)
        nexus_config_dir = self.task_config.get('NEXUS_CONFIG_DIR', None)
        nexus_input_dir = self.task_config.get('NEXUS_INPUT_DIR', None)

        # Default NEXUS_TSTEP to 3600 seconds (1 hour) if not set
        nexus_tstep = self.task_config.get('NEXUS_TSTEP', 3600)

        logger.info(f"Using NEXUS_CONFIG: {nexus_config_set}")
        logger.info(f"Using NEXUS_CONFIG_DIR: {nexus_config_dir}")
        logger.info(f"Using NEXUS_INPUT_DIR: {nexus_input_dir}")
        logger.info(f"Using NEXUS_TSTEP: {nexus_tstep}")

        # Check for grid parameters
        required_grid_params = [
            'NEXUS_NX',
            'NEXUS_NY',
            'NEXUS_NZ',
            'NEXUS_XMIN',
            'NEXUS_XMAX',
            'NEXUS_YMIN',
            'NEXUS_YMAX'
        ]
        for param in required_grid_params:
            if not self.task_config.get(param, None):
                raise WorkflowException(f"{param} must be set in task configuration")

        logger.info(f"Grid parameters: NEXUS_NX={self.task_config.NEXUS_NX}")
        logger.info(f"Grid parameters: NEXUS_NY={self.task_config.NEXUS_NY}")
        logger.info(f"Grid parameters: NEXUS_NZ={self.task_config.NEXUS_NZ}")
        logger.info(f"Grid parameters: NEXUS_XMIN={self.task_config.NEXUS_XMIN}")
        logger.info(f"Grid parameters: NEXUS_XMAX={self.task_config.NEXUS_XMAX}")
        logger.info(f"Grid parameters: NEXUS_YMIN={self.task_config.NEXUS_YMIN}")
        logger.info(f"Grid parameters: NEXUS_YMAX={self.task_config.NEXUS_YMAX}")

        processed_nexus_files = []
        final_output_files = []
        sorted_dates = sorted(self.forecast_dates)
        for d in sorted_dates[:-1]:
            fname = f"{self.task_config.NEXUS_DIAG_PREFIX}.{d.strftime('%Y%m%d%H')}00.nc"
            fname_final = f"{self.task_config.NEXUS_DIAG_PREFIX}.{d.strftime('%Y%m%d')}.nc"
            processed_nexus_files.append(fname)
            final_output_files.append(fname_final)
        final_output_files = list(set(final_output_files))
        logger.info(f"Final output files: {final_output_files}")
        self.processed_nexus_files = processed_nexus_files
        # render the NEXUS configuration files
        if not os.path.exists(nexus_config_dir):
            raise WorkflowException(f"NEXUS configuration file not found: {nexus_config_dir}")
        logger.info(f"Rendering NEXUS configuration from {nexus_config_dir}")
        tmpl_dict = {
            'NEXUS_CONFIG': nexus_config_set,
            'NEXUS_CONFIG_DIR': nexus_config_dir,
            'NEXUS_INPUT_DIR': nexus_input_dir,
            'NEXUS_DIAG_PREFIX': self.task_config.NEXUS_DIAG_PREFIX,
            'NEXUS_TSTEP': nexus_tstep,
            'NEXUS_NX': self.task_config.NEXUS_NX,
            'NEXUS_NY': self.task_config.NEXUS_NY,
            'NEXUS_NZ': self.task_config.NEXUS_NZ,
            'NEXUS_XMIN': self.task_config.NEXUS_XMIN,
            'NEXUS_XMAX': self.task_config.NEXUS_XMAX,
            'NEXUS_YMIN': self.task_config.NEXUS_YMIN,
            'NEXUS_YMAX': self.task_config.NEXUS_YMAX,
            'LOCAL_INPUT_DIR': os.path.join(self.task_config.DATA, 'INPUT'),
            'NEXUS_EXECUTABLE': os.path.join(self.task_config.get('HOMEgfs', None), "exec/nexus.x"),
            "DATA": self.task_config.DATA,
            "NEXUS_DO_MEGAN": self.task_config.get('NEXUS_DO_MEGAN', False),
            "NEXUS_DO_CEDS2019": self.task_config.get('NEXUS_DO_CEDS2019', True),
            "NEXUS_DO_CEDS2024": self.task_config.get('NEXUS_DO_CEDS2024', False),
            "NEXUS_DO_HTAPv2": self.task_config.get('NEXUS_DO_HTAPv2', True),
            "NEXUS_DO_HTAPv3": self.task_config.get('NEXUS_DO_HTAPv3', False),
            "NEXUS_DO_CAMS": self.task_config.get('NEXUS_DO_CAMS', False),
            "NEXUS_DO_CAMSTEMPO": self.task_config.get('NEXUS_DO_CAMSTEMPO', False),
            "start_date": self.start_date.strftime('%Y-%m-%d %H:%M:%S'),
            "end_date": self.end_date.strftime('%Y-%m-%d %H:%M:%S'),
            "FINAL_OUTPUT": final_output_files,
            "COMOUT_CHEM_INPUT": self.task_config.COMOUT_CHEM_INPUT,
            "COMOUT_CHEM_RESTART": self.task_config.COMOUT_CHEM_RESTART,
            "RestartFile": f"HEMCO_restart.{self.end_date.strftime('%Y%m%d%H')}00.nc",
            "processed_nexus_files": processed_nexus_files,

        }

        yaml_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'chem', 'nexus_emission.yaml.j2')
        if not os.path.exists(yaml_template):
            logger.warning(f"Template file not found: {yaml_template}, using default configuration")
            yaml_config = {'nexus_emission': {}}
        else:
            logger.debug(f'Parsing YAML template: {yaml_template}')
            yaml_config = parse_j2yaml(yaml_template, tmpl_dict)

        # Add yaml configuration to task_config
        self.task_config = AttrDict(**self.task_config, **yaml_config)

        # Link NEXUS input directory to the working directory
        FileHandler(self.task_config.nexus_emission.data_in).sync()
        logger.info(f"NEXUS input directory linked to {self.task_config.DATA}")

        # Render NEXUS Grid File
        file_loader = FileSystemLoader(self.task_config.NEXUS_CONFIG_DIR)
        env = Environment(loader=file_loader)
        nexus_grid_template = env.get_template(f"{self.task_config.NEXUS_GRID_NAME}.j2")
        self.task_config.NEXUS_GRID_TEMPLATE = nexus_grid_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_GRID_NAME)
        _write_txt_file(self.task_config.NEXUS_GRID_TEMPLATE, outfile)
        logger.info(f"NEXUS grid file rendered successfully: written to {outfile}")

        # Render NEXUS Config File
        nexus_config_template = env.get_template(f"{self.task_config.NEXUS_CONFIG_NAME}.j2")
        self.task_config.NEXUS_CONFIG_TEMPLATE = nexus_config_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_CONFIG_NAME)
        _write_txt_file(self.task_config.NEXUS_CONFIG_TEMPLATE, outfile)
        logger.info(f"NEXUS config file rendered successfully: written to {outfile}")

        # Render NEXUS Time File
        nexus_time_template = env.get_template(f"{self.task_config.NEXUS_TIME_NAME}.j2")
        self.task_config.NEXUS_TIME_TEMPLATE = nexus_time_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_TIME_NAME)
        _write_txt_file(self.task_config.NEXUS_TIME_TEMPLATE, outfile)
        logger.info(f"NEXUS time file rendered successfully: written to {outfile}")

        # Render NEXUS Diag File
        nexus_diag_template = env.get_template(f"{self.task_config.NEXUS_DIAG_NAME}.j2")
        self.task_config.NEXUS_DIAG_TEMPLATE = nexus_diag_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_DIAG_NAME)
        _write_txt_file(self.task_config.NEXUS_DIAG_TEMPLATE, outfile)
        logger.info(f"NEXUS diag file rendered successfully: written to {outfile}")

        # Render NEXUS Spec File
        nexus_spec_template = env.get_template(f"{self.task_config.NEXUS_SPEC_NAME}.j2")
        self.task_config.NEXUS_SPEC_TEMPLATE = nexus_spec_template.render(tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_SPEC_NAME)
        _write_txt_file(self.task_config.NEXUS_SPEC_TEMPLATE, outfile)
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

        exe = Executable(self.task_config.APRUN)
        arg_list = [ './nexus.x', '-c',  self.task_config.NEXUS_CONFIG_NAME]
        exe(*arg_list, output='stdout', error='stderr')

        logger.info("Concatenating processed NEXUS files...")

        # sort the files even though they should be sorted already | safety check
        files = sorted(self.processed_nexus_files)

        for i in files:

            if not os.path.exists(i):
                logger.warning(f"NEXUS file not found: {i}")
                continue
            else:
                logger.info(f"NEXUS file found: {i}")

        for f, dates in zip(files, self.forecast_dates):
            logger.info(f" - {f}, {dates}")

        # find the day indexes for each unique day
        # this returns a dictionary
        # example:
        # {
        #     datetime.date(2024, 1, 5): [0, 1, 3],
        #     datetime.date(2024, 1, 6): [2]
        # }
        day_indexes = _get_day_indices(self.forecast_dates[:-1])  # hemco doesn't write out the last timestep
        # now loop over each days
        for date, indexes in day_indexes.items():
            day_str = date.strftime('%Y%m%d')
            logger.info(f"Processing NEXUS files for date: {date}")

            dsets = []
            for index in indexes:
                # list files for log
                logger.info(f" - {files[index]}, {index}")

                # now concatenate the files per day
                if os.path.exists(files[index]) is False:
                    break
                ds = xr.open_dataset(files[index], decode_cf=False)

                # update time coordinate
                ds = ds.assign_coords(time=('time', [index]))

                # set time units to reference start-date
                ds.time.attrs['units'] = self.start_date.strftime('hours since %Y-%m-%d %H:00:00')

                # append
                dsets.append(ds)

            # concatenate all the files for this day
            if dsets is None:
                break
            else:
                ds = xr.concat(dsets, dim='time')

            encoding = {var: {"zlib": True, "complevel": 2} for var in ds.data_vars}
            outname = f"{self.task_config.NEXUS_DIAG_PREFIX}.{day_str}.nc"
            ds.to_netcdf(outname, format="NETCDF4", encoding=encoding)
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

        FileHandler(self.task_config.nexus_emission.data_out).sync()

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


def _get_day_indices(datetimes):
    """
    Group indices of datetimes by day, including midnight in both days.

    Parameters
    ----------
    datetimes : list of datetime.datetime
        List of datetime objects.

    Returns
    -------
    dict
        Dictionary mapping datetime.datetime (at midnight) to list of indices.
        Each day includes all hours from 00:00 of that day through 00:00 of the next day,
        and the midnight index is included in both days.
    """
    from collections import defaultdict
    from datetime import timedelta

    grouped = defaultdict(list)

    for idx, dt in enumerate(datetimes):
        day_dt = dt.replace(hour=0, minute=0, second=0, microsecond=0)
        grouped[day_dt].append(idx)
        # If this is exactly midnight, also add to previous day
        if dt.hour == 0 and dt.minute == 0 and dt.second == 0 and dt.microsecond == 0:
            prev_day = day_dt - timedelta(days=1)
            grouped[prev_day].append(idx)

    return dict(grouped)
