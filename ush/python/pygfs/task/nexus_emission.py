#!/usr/bin/env python3

import os
import re
from pathlib import Path
from collections import defaultdict
import xarray as xr
from logging import getLogger
from datetime import datetime, timedelta
from typing import Dict, Any, Union, List
from dateutil.rrule import DAILY, HOURLY, rrule
from collections import defaultdict
from datetime import timedelta
from wxflow import (AttrDict,
                    FileHandler,
                    parse_j2yaml,
                    logit,
                    Task,
                    Jinja,
                    to_timedelta,
                    WorkflowException,
                    Executable)

# Try to import toml, fail gracefully if not installed
try:
    import toml
except ImportError:
    try:
        import tomllib as toml  # Python 3.11+
    except ImportError:
        class DummyTOML:
            def load(self, f):
                return {}
        toml = DummyTOML()

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

        # self.task_config = AttrDict(config)
        self.AERO_INPUTS_DIR = self.task_config.get('AERO_INPUTS_DIR', None)
        self.COMOUT_CHEM_INPUT = self.task_config.get('COMOUT_CHEM_INPUT', None)

        # get the nforecast hours - gcdas will use FHMAX and gcafs will use FHMAX_GFS
        if 'das' in self.task_config['RUN']:
            nforecast_hours = self.task_config["FHMAX"]
        else:
            nforecast_hours = self.task_config["FHMAX_GFS"]

        self.start_date = self.task_config["current_cycle"]
        self.total_hrs = nforecast_hours + 1
        self.end_date = self.task_config["current_cycle"] + to_timedelta(f'{self.total_hrs}H')

        logger.info(f'start_date: {self.start_date}')
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
            "nmem_ens": self.task_config.NMEM_ENS,
        }

        # Render NEXUS Grid File
        nexus_grid_template = os.path.join(nexus_config_dir, f"{self.task_config.NEXUS_GRID_NAME}.j2")
        logger.info(f"Rendering NEXUS grid file using template: {nexus_grid_template}")
        if not os.path.exists(nexus_grid_template):
            raise WorkflowException(f"NEXUS grid template file not found: {nexus_grid_template}")
        j2_renderer = Jinja(nexus_grid_template, tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_GRID_NAME)
        j2_renderer.save(outfile)
        logger.info(f"NEXUS grid file rendered successfully: written to {outfile}")

        # Render NEXUS Config File
        nexus_config_template = os.path.join(nexus_config_dir, f"{self.task_config.NEXUS_CONFIG_NAME}.j2")
        logger.info(f"Rendering NEXUS config file using template: {nexus_config_template}")
        if not os.path.exists(nexus_config_template):
            raise WorkflowException(f"NEXUS config template file not found: {nexus_config_template}")
        j2_renderer = Jinja(nexus_config_template, tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_CONFIG_NAME)
        j2_renderer.save(outfile)
        logger.info(f"NEXUS config file rendered successfully: written to {outfile}")

        # Render NEXUS Time File
        nexus_time_template = os.path.join(nexus_config_dir, f"{self.task_config.NEXUS_TIME_NAME}.j2")
        logger.info(f"Rendering NEXUS time file using template: {nexus_time_template}")
        if not os.path.exists(nexus_time_template):
            raise WorkflowException(f"NEXUS time template file not found: {nexus_time_template}")
        j2_renderer = Jinja(nexus_time_template, tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_TIME_NAME)
        j2_renderer.save(outfile)
        logger.info(f"NEXUS time file rendered successfully: written to {outfile}")

        # Render NEXUS Diag File
        nexus_diag_template = os.path.join(nexus_config_dir, f"{self.task_config.NEXUS_DIAG_NAME}.j2")
        logger.info(f"Rendering NEXUS diag file using template: {nexus_diag_template}")
        if not os.path.exists(nexus_diag_template):
            raise WorkflowException(f"NEXUS diag template file not found: {nexus_diag_template}")
        j2_renderer = Jinja(nexus_diag_template, tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_DIAG_NAME)
        j2_renderer.save(outfile)
        logger.info(f"NEXUS diag file rendered successfully: written to {outfile}")

        # Render NEXUS Spec File
        nexus_spec_template = os.path.join(nexus_config_dir, f"{self.task_config.NEXUS_SPEC_NAME}.j2")
        logger.info(f"Rendering NEXUS spec file using template: {nexus_spec_template}")
        if not os.path.exists(nexus_spec_template):
            raise WorkflowException(f"NEXUS spec template file not found: {nexus_spec_template}")
        j2_renderer = Jinja(nexus_spec_template, tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_SPEC_NAME)
        j2_renderer.save(outfile)
        logger.info(f"NEXUS spec file rendered successfully: written to {outfile}")

        # find needed inputs
        found_files, missing_files, root_path = gather_emissions_files_from_time_file(
            hemco_config_path=os.path.join(self.task_config.DATA, self.task_config.NEXUS_CONFIG_NAME),
            hemco_time_path=os.path.join(self.task_config.DATA, self.task_config.NEXUS_TIME_NAME),
        )
        if len(missing_files) > 0:
            for mf in missing_files:
                logger.error(f"Missing NEXUS emission input file: {mf}")
            raise WorkflowException(f"Missing {len(missing_files)} NEXUS emission input files, cannot proceed")

        tmpl_dict["NEXUS_INPUT_FILES"] = found_files
        tmpl_dict["NEXUS_COPY_TO_FILES"] = [os.path.join(self.task_config.DATA, 'INPUT', os.path.relpath(f, root_path)) for f in found_files]
        tmpl_dict["NEXUS_INPUT_DIR"] = os.path.join(self.task_config.DATA, 'INPUT')
        # Create all necessary directories for the destination files
        for dest_file in tmpl_dict["NEXUS_COPY_TO_FILES"]:
            dest_dir = os.path.dirname(dest_file)
            os.makedirs(dest_dir, exist_ok=True)

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

        # Rerender NEXUS config files with updated input files
        # Render NEXUS Config File
        nexus_config_template = os.path.join(nexus_config_dir, f"{self.task_config.NEXUS_CONFIG_NAME}.j2")
        logger.info(f"Rendering NEXUS config file using template: {nexus_config_template}")
        if not os.path.exists(nexus_config_template):
            raise WorkflowException(f"NEXUS config template file not found: {nexus_config_template}")
        j2_renderer = Jinja(nexus_config_template, tmpl_dict)
        outfile = os.path.join(self.task_config.DATA, self.task_config.NEXUS_CONFIG_NAME)
        j2_renderer.save(outfile)
        logger.info(f"NEXUS config file rendered successfully: written to {outfile}")

        # create a directory in the self.task_config.DATA/Restarts
        os.makedirs(os.path.join(self.task_config.DATA, 'Restarts'), exist_ok=True)
        logger.info(f"Created Restarts directory: {os.path.join(self.task_config.DATA, 'Restarts')}")

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

        if os.path.exists("nexus.x") is False:
            raise WorkflowException("NEXUS preprocessor executable 'nexus.x' not found in PATH")

        arg_list = ['./nexus.x', '-c', self.task_config.NEXUS_CONFIG_NAME]
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
            if len(dsets) == 0:
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


def _get_day_indices(datetimes: List[datetime]) -> Dict[datetime, List[int]]:
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

    grouped = defaultdict(list)

    for idx, dt in enumerate(datetimes):
        day_dt = dt.replace(hour=0, minute=0, second=0, microsecond=0)
        grouped[day_dt].append(idx)
        # If this is exactly midnight, also add to previous day
        if dt.hour == 0 and dt.minute == 0 and dt.second == 0 and dt.microsecond == 0:
            prev_day = day_dt - timedelta(days=1)
            grouped[prev_day].append(idx)

    return dict(grouped)


def daterange(start_date: datetime, end_date: datetime):
    """
    Generate dates from start_date to end_date inclusive.

    Parameters
    ----------
    start_date : datetime
        Start date (inclusive).
    end_date : datetime
        End date (inclusive).

    Yields
    ------
    datetime
        Each date in the range.
    """
    for n in range(int((end_date - start_date).days) + 1):
        yield start_date + timedelta(n)


def parse_year_bounds(hemco_time_str: str) -> tuple[None | int, None | int]:
    """
    Parses a HEMCO time string for year bounds.

    Parameters
    ----------
    hemco_time_str : str
        HEMCO time string, e.g. '2000-2022/1-12/1/0'.

    Returns
    -------
    tuple of (int or None, int or None)
        (start_year, end_year) or (None, None) if wildcard or invalid.
    """
    if not hemco_time_str or hemco_time_str.strip() == "*":
        return None, None

    date_part = hemco_time_str.split('/')[0].strip()
    if "-" in date_part:
        try:
            parts = date_part.split('-')
            start_y = int(parts[0])
            end_y = int(parts[-1])
            return start_y, end_y
        except ValueError:
            return None, None
    else:
        try:
            val = int(date_part)
            return val, val
        except ValueError:
            return None, None


def resolve_variables(path: str, var_definitions: dict[str, str]) -> str:
    """
    Replace variables in a path string using a dictionary.

    Parameters
    ----------
    path : str
        Path string with variables (e.g., $ROOT).
    var_definitions : dict of str
        Dictionary of variable names to values.

    Returns
    -------
    str
        Path with variables replaced.
    """
    resolved_path = path.replace("$$", "$")
    for key in sorted(var_definitions.keys(), key=len, reverse=True):
        if key in resolved_path:
            resolved_path = resolved_path.replace(key, var_definitions[key])
    return resolved_path


def expand_filenames(
    file_template: str,
    hemco_time_str: str,
    sector_conf: dict[str, Any],
    start_date: datetime,
    end_date: datetime
) -> set[str]:
    """
    Expand file templates into actual filenames for a date range and frequency.

    Parameters
    ----------
    file_template : str
        Template for file names (e.g., "$YYYY$MM$DD.nc").
    hemco_time_str : str
        HEMCO time string for year bounds.
    sector_conf : dict
        Sector configuration, must contain 'frequency' and optionally 'patterns'.
    start_date : datetime
        Start date for expansion.
    end_date : datetime
        End date for expansion.

    Returns
    -------
    set of str
        Set of expanded file names.
    """
    generated_files = set()
    freq = sector_conf.get("frequency", "monthly")
    patterns = sector_conf.get("patterns", [])

    min_year, max_year = parse_year_bounds(hemco_time_str)

    def get_effective_year(target_year: int) -> str:
        if max_year is not None and target_year > max_year:
            return str(max_year)
        if min_year is not None and target_year < min_year:
            return str(min_year)
        return str(target_year)

    if freq == "daily":
        for single_date in daterange(start_date, end_date):
            eff_year = get_effective_year(single_date.year)
            mm = f"{single_date.month:02d}"
            dd = f"{single_date.day:02d}"
            fname = file_template.replace("$YYYY", eff_year).replace("$MM", mm).replace("$DD", dd)
            generated_files.add(fname)
    elif freq == "monthly":
        unique_months = set((d.year, d.month) for d in daterange(start_date, end_date))
        for (year, month) in unique_months:
            eff_year = get_effective_year(year)
            mm = f"{month:02d}"
            fname = file_template.replace("$YYYY", eff_year).replace("$MM", mm)
            if "$YYYY" not in file_template:
                fname = fname.replace("$YYYY", "")
            generated_files.add(fname)
    elif freq == "representative":
        unique_months = set((d.year, d.month) for d in daterange(start_date, end_date))
        for (year, month) in unique_months:
            eff_year = get_effective_year(year)
            mm = f"{month:02d}"
            base_name = file_template.replace("$YYYY", eff_year).replace("$MM", mm)
            for pat in patterns:
                if "$DAY" in base_name:
                    fname = base_name.replace("$DAY", pat)
                elif "$D" in base_name:
                    fname = base_name.replace("$D", pat)
                else:
                    fname = base_name
                generated_files.add(fname)
    else:
        unique_years = set(d.year for d in daterange(start_date, end_date))
        for year in unique_years:
            if "$YYYY" not in file_template:
                generated_files.add(file_template)
            else:
                eff_year = get_effective_year(year)
                fname = file_template.replace("$YYYY", eff_year)
                generated_files.add(fname)
    return generated_files


def extract_dataset_name(file_path: str, root_path: str) -> str:
    """
    Extract dataset name from file path by finding the directory after ROOT.

    Parameters
    ----------
    file_path : str
        Full file path.
    root_path : str
        Root directory path.

    Returns
    -------
    str
        Dataset name or "unknown" if not found.
    """
    try:
        rel_path = os.path.relpath(file_path, root_path)
        parts = rel_path.split(os.sep)
        if len(parts) > 0:
            return parts[0]
    except ValueError:
        parts = file_path.split(os.sep)
        for i, part in enumerate(parts):
            if part in ['nexus', 'emissions', 'data'] and i < len(parts) - 1:
                return parts[i + 1]
    return "unknown"


def copy_files_with_structure(
    file_list: list[str],
    root_path: str,
    copy_dir: str
) -> tuple[int, int]:
    """
    Copy files to local directory maintaining dataset structure.

    Parameters
    ----------
    file_list : list of str
        List of file paths to copy.
    root_path : str
        Root directory path to strip from file paths.
    copy_dir : str
        Directory to copy files to.

    Returns
    -------
    tuple of (int, int)
        (copied_count, failed_count)
    """
    copied_count = 0
    failed_count = 0
    os.makedirs(copy_dir, exist_ok=True)
    for file_path in file_list:
        try:
            dataset_name = extract_dataset_name(file_path, root_path)
            try:
                rel_path = os.path.relpath(file_path, root_path)
            except ValueError:
                rel_path = file_path.lstrip('/')
            dest_path = os.path.join(copy_dir, rel_path)
            dest_dir = os.path.dirname(dest_path)
            os.makedirs(dest_dir, exist_ok=True)
            shutil.copy2(file_path, dest_path)
            copied_count += 1
        except Exception as e:
            failed_count += 1
    return copied_count, failed_count


def parse_hemco(
    rc_path: str,
    toml_path: str,
    start_date: datetime,
    end_date: datetime
) -> tuple[list[str], str | None]:
    """
    Parse HEMCO config and return (file_list, root_path).

    Parameters
    ----------
    rc_path : str
        Path to HEMCO config file.
    toml_path : str
        Path to TOML rules file.
    start_date : datetime
        Start date for file search.
    end_date : datetime
        End date for file search.

    Returns
    -------
    tuple
        (file_list, root_path)
        file_list : list of str
            List of emission file paths.
        root_path : str or None
            Root directory path from config.
    """

    # Load sector rules with better defaults
    try:
        with open(toml_path, 'r') as tf:
            sector_rules = toml.load(tf)
    except Exception:
        # Create default rules for common patterns
        sector_rules = {
            "default": {"frequency": "monthly"},
            "CEDS": {"frequency": "yearly"},
            "GFED": {"frequency": "daily"},
            "FINN": {"frequency": "daily"}
        }

    defined_vars = {}
    all_files = set()
    enabled_extensions = set()
    enabled_collections = set()
    root_path = None

    var_pattern = re.compile(r'^\s*([A-Za-z0-9_]+)\s*:\s*(.*)')
    data_sections = ["BASE EMISSIONS", "SCALE FACTORS", "MASKS"]
    current_section = None
    in_conditional_section = None

    with open(rc_path, 'r') as f:
        lines = f.readlines()

    for line in lines:
        raw = line.strip()

        # Handle comments, but allow section headers that start with ###
        if not raw:
            continue
        if (raw.startswith("!") or
                (raw.startswith("#") and "BEGIN SECTION" not in raw and "END SECTION" not in raw)):
            continue

        # Section Detection
        if "BEGIN SECTION" in raw:
            if "SETTINGS" in raw:
                current_section = "SETTINGS"
            elif "EXTENSION SWITCHES" in raw:
                current_section = "EXTENSION SWITCHES"
            else:
                for s in data_sections:
                    if s in raw:
                        current_section = s
            continue
        if "END SECTION" in raw:
            current_section = None
            continue

        # Handle conditional sections like (((CEDS and )))CEDS
        if raw.startswith("((("):
            collection_name = raw[3:]
            in_conditional_section = collection_name
            continue
        if raw.startswith(")))"):
            in_conditional_section = None
            continue

        # Extension Switches - parse to determine what's enabled
        if current_section == "EXTENSION SWITCHES":
            if "-->" in raw and ":" in raw:
                # Format: --> CEDS : on true
                parts = raw.split(":")
                if len(parts) >= 2:
                    ext_name = parts[0].strip().replace("-->", "").strip()
                    ext_value = parts[1].strip().lower()
                    if "on" in ext_value and "true" in ext_value:
                        enabled_collections.add(ext_name)
            continue

        # Settings
        if current_section == "SETTINGS":
            match = var_pattern.match(raw)
            if match:
                k, v = match.groups()
                clean_val = v.split('!')[0].split('#')[0].strip()
                defined_vars[f"${k}"] = clean_val
                # Capture ROOT path for copying functionality
                if k == "ROOT":
                    root_path = clean_val
            continue

        # Data Sections
        if current_section in data_sections:
            # Skip if we're in a conditional section that's not enabled
            if in_conditional_section and in_conditional_section not in enabled_collections:
                continue

            parts = raw.split()

            # Handle different section formats
            if len(parts) >= 5:
                if current_section == "BASE EMISSIONS":
                    # Format: ExtNr Name sourceFile sourceVar sourceTime ...
                    ext_nr = parts[0]
                    name = parts[1]
                    raw_file = parts[2]
                    source_var = parts[3]
                    raw_time = parts[4]

                    # Skip disabled extensions (only process extension 0 and *)
                    if ext_nr != "0" and ext_nr != "*":
                        continue

                elif current_section == "SCALE FACTORS":
                    # Format: ScalID Name sourceFile sourceVar sourceTime ...
                    scale_id = parts[0]
                    name = parts[1]
                    raw_file = parts[2]
                    source_var = parts[3]
                    raw_time = parts[4]

                else:
                    # Other sections - try to parse similar format
                    ext_nr = parts[0]
                    name = parts[1]
                    raw_file = parts[2]
                    source_var = parts[3]
                    raw_time = parts[4]

                # Common filtering for all sections
                # Filtering garbage
                if raw_file == '-' or raw_file.startswith("MATH:") or raw_file.upper() == "MASK" or raw_file == "1.0":
                    continue
                if not any(c.isalpha() or c == '$' or c == '/' for c in raw_file):
                    continue

                # 1. Resolve Variables
                resolved_path = resolve_variables(raw_file, defined_vars)

                # 2. Get Rules - try exact name match first, then collection, then default
                rules = sector_rules.get(
                    name,
                    sector_rules.get(
                        in_conditional_section if in_conditional_section else "default",
                        sector_rules.get("default", {})
                    )
                )

                # 3. Expand with Year Clamping
                files = expand_filenames(resolved_path, raw_time, rules, start_date, end_date)
                all_files.update(files)

    return sorted(list(all_files)), root_path


def parse_hemco_time_file(time_file_path: str) -> tuple[datetime | None, datetime | None]:
    """
    Parse HEMCO_sa_Time.rc to extract start and end dates.

    Parameters
    ----------
    time_file_path : str
        Path to HEMCO time file.

    Returns
    -------
    tuple
        (start_date, end_date) as datetime or None if not found.
    """
    start_date = None
    end_date = None
    if os.path.exists(time_file_path):
        with open(time_file_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line.startswith('START:'):
                    date_str = line.split(':')[1].strip().split()[0]
                    start_date = datetime.strptime(date_str, '%Y-%m-%d')
                elif line.startswith('END:'):
                    date_str = line.split(':')[1].strip().split()[0]
                    end_date = datetime.strptime(date_str, '%Y-%m-%d')
    return start_date, end_date


def gather_emissions_files(
    hemco_config_path: str,
    start_date: datetime,
    end_date: datetime,
    toml_rules_path: str = "nexus_sectors.toml",
    verbose: bool = False
) -> tuple[list[str], list[str], str | None]:
    """
    Main API function to gather emissions files for a date range.

    Parameters
    ----------
    hemco_config_path : str
        Path to HEMCO config file (e.g., NEXUS_Config.rc)
    start_date : datetime
        Start date for file search
    end_date : datetime
        End date for file search
    toml_rules_path : str, optional
        Path to TOML rules file
    verbose : bool, optional
        Print detailed progress information

    Returns
    -------
    tuple
        (found_files, missing_files, root_path)
        found_files : list of str
            List of existing file paths
        missing_files : list of str
            List of missing file paths
        root_path : str or None
            ROOT directory path from config
    """
    if not os.path.exists(hemco_config_path):
        raise FileNotFoundError(f"HEMCO config file not found: {hemco_config_path}")
    potential_files, root_path = parse_hemco(hemco_config_path, toml_rules_path, start_date, end_date)
    found = []
    missing = []
    for fpath in potential_files:
        clean = os.path.expanduser(fpath)
        if os.path.exists(clean) and os.path.isfile(clean):
            found.append(clean)
        else:
            missing.append(clean)
    return found, missing, root_path


def gather_emissions_files_from_time_file(
    hemco_config_path: str,
    hemco_time_path: str,
    toml_rules_path: str = "nexus_sectors.toml",
    verbose: bool = False
) -> tuple[list[str], list[str], str | None]:
    """
    Gather emissions files using dates from HEMCO time file.

    Parameters
    ----------
    hemco_config_path : str
        Path to HEMCO config file
    hemco_time_path : str
        Path to HEMCO time file (e.g., HEMCO_sa_Time.rc)
    toml_rules_path : str, optional
        Path to TOML rules file
    verbose : bool, optional
        Print detailed progress information

    Returns
    -------
    tuple
        (found_files, missing_files, root_path)
        found_files : list of str
            List of existing file paths
        missing_files : list of str
            List of missing file paths
        root_path : str or None
            ROOT directory path from config
    """
    start_date, end_date = parse_hemco_time_file(hemco_time_path)
    if not start_date or not end_date:
        raise ValueError(f"Could not parse dates from time file: {hemco_time_path}")
    return gather_emissions_files(hemco_config_path, start_date, end_date, toml_rules_path, verbose)


def copy_emissions_files(
    file_list: list[str],
    root_path: str,
    destination_dir: str,
    verbose: bool = False
) -> tuple[int, int]:
    """
    Copy emission files to local directory with organized structure.

    Parameters
    ----------
    file_list : list of str
        List of file paths to copy
    root_path : str
        ROOT directory path to strip from file paths
    destination_dir : str
        Directory to copy files to
    verbose : bool, optional
        Print detailed progress information

    Returns
    -------
    tuple
        (copied_count, failed_count)
    """
    if not file_list:
        return 0, 0
    if not root_path:
        root_path = "/"
    return copy_files_with_structure(file_list, root_path, destination_dir)


def save_file_list(file_list: list[str], output_path: str) -> None:
    """
    Save file list to text file.

    Parameters
    ----------
    file_list : list of str
        List of file paths
    output_path : str
        Output file path
    """
    with open(output_path, "w") as f:
        for fpath in file_list:
            f.write(fpath + "\n")
