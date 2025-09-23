#!/usr/bin/env python3

import os
import re
import fnmatch
import datetime
import xarray as xr
import numpy as np
from logging import getLogger
from typing import Dict, Any, Union, List
from dateutil.rrule import DAILY, rrule
from wxflow import (AttrDict,
                    parse_j2yaml,
                    FileHandler,
                    logit,
                    Task,
                    to_timedelta,
                    WorkflowException,
                    Executable, which)

logger = getLogger(__name__.split('.')[-1])


class ChemFireEmissions(Task):
    """Chemistry Emissions pre-processing Task
    """

    @logit(logger, name="ChemFireEmissions")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Chemistry Fire Emissions task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        self.historical = bool(self.task_config.get('AERO_EMIS_FIRE_HIST', 1))
        self.AERO_INPUTS_DIR = self.task_config.get('AERO_INPUTS_DIR', None)
        self.COMOUT_CHEM_INPUT = self.task_config.get('COMOUT_CHEM_INPUT', None)

        # get the nforecast hours - gcdas will use FHMAX and gcafs will use FHMAX_GFS
        if 'das' in self.task_config['RUN']:
            nforecast_hours = self.task_config["FHMAX"]
        else:
            nforecast_hours = self.task_config["FHMAX_GFS"]
        logger.info(f"Number of forecast hours: {nforecast_hours}")

        # Create start date based on SDATE
        self.start_date = self.task_config["CDATE"]
        logger.info(f"Start date: {self.start_date}")

        # end date = SDATE + nforecast hours + 36
        self.end_date = self.task_config["CDATE"] + to_timedelta(f'{nforecast_hours + 36}H')
        logger.info(f"End date: {self.end_date}")

        # Calculate number of days spanned by start and end date (inclusive)
        numdays = (self.end_date.date() - self.start_date.date()).days + 1
        logger.info(f"Number of days in forecast period: {numdays}")

        self.forecast_dates = list(rrule(freq=DAILY, dtstart=self.start_date, count=numdays))
        logger.info(f"Forecast dates: {self.forecast_dates}")

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the work directory and process chemical emissions configuration.

        This method performs the following steps:
        1. Loads and parses the fire_emission.yaml.j2 template
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
        - AERO_EMIS_FIRE_VERSION : str
            Version of fire emission data (GBBEPx or QFED)

        The configuration is processed through a Jinja2 template system
        and the resulting setup is stored in self.task_config.
        """

        if self.historical:
            logger.info(f'Processing historical emissions for {self.start_date} to {self.end_date}')

            # print(self.task_config)
            aero_inputs_dir = str(self.task_config.AERO_INPUTS_DIR)
            aero_emis_fire = str(self.task_config.AERO_EMIS_FIRE)
            aero_emis_fire_version = str(self.task_config.AERO_EMIS_FIRE_VERSION)

            logger.info(f'Using AERO_INPUTS_DIR: {aero_inputs_dir}')
            logger.info(f'Using AERO_EMIS_FIRE: {aero_emis_fire}')
            logger.info(f'Using AERO_EMIS_FIRE_VERSION: {aero_emis_fire_version}')

            AERO_EMIS_FIRE_DIR = os.path.join(aero_inputs_dir,
                                              "nexus",
                                              aero_emis_fire.upper())

            logger.info(f'Final AERO_EMIS_FIRE_DIR: {AERO_EMIS_FIRE_DIR}')

            # find the forecast dates that are in the historical period for the given emission dataset
            files_found = []
            for dates in self.forecast_dates:
                if self.task_config.AERO_EMIS_FIRE.lower() == 'gbbepx':
                    gbbepx_vars = self.task_config.get('gbbepx_vars', ["co", "nox", "so2", "nh3", "bc", "oc"])
                    files = self._find_gbbepx_files(dates,
                                                    gbbepx_vars,
                                                    version=self.task_config.AERO_EMIS_FIRE_VERSION,
                                                    aero_emis_fire_dir=AERO_EMIS_FIRE_DIR)
                elif self.task_config.AERO_EMIS_FIRE.lower() == 'qfed':

                    qfed_vars = self.task_config.get('qfed_vars', ["co", "nox", "so2", "nh3", "bc", "oc"])
                    files = self._find_qfed_files(dates,
                                                  qfed_vars,
                                                  version=self.task_config.AERO_EMIS_FIRE_VERSION,
                                                  aero_emis_fire_dir=AERO_EMIS_FIRE_DIR)
                files_found.extend(files)
            logger.info(f'Found {len(files_found)} files for historical period')
        else:
            logger.info(f'Processing forecast emissions for {self.start_date}')

            if self.task_config.AERO_EMIS_FIRE.lower() == 'gbbepx':
                files = self._find_gbbepx_files(
                    self.start_date,
                    self.task_config.gbbepx_vars,
                    version=self.task_config.AERO_EMIS_FIRE_VERSION,
                    vars=self.task_config.get('gbbepx_vars', ["co", "nox", "so2", "nh3", "bc", "oc"])
                )
            elif self.task_config.AERO_EMIS_FIRE.lower() == 'qfed':
                # Get QFED variables with safe defaults
                qfed_vars = self.task_config.get('qfed_vars', ["co", "nox", "so2", "nh3", "bc", "oc"])
                if isinstance(qfed_vars, str):
                    qfed_vars = qfed_vars.split()
                # Ensure version is properly formatted
                version = self.task_config.AERO_EMIS_FIRE_VERSION
                if isinstance(version, int) or version.isdigit():
                    version = str(version).zfill(3)  # Pad with leading zeros if needed

                # Get fire emissions directory
                aero_emis_fire_dir = getattr(self.task_config, 'AERO_EMIS_FIRE_DIR', None)

                files = self._find_qfed_files(
                    self.start_date,
                    vars=qfed_vars,
                    version=version,
                    aero_emis_fire_dir=aero_emis_fire_dir
                )

        # Fill the COMOUT_CHEM_INPUT with environment variables to create the full output path
        processed_files = []
        for dt in self.forecast_dates:
            processed_files.append(
                dt.strftime("FIRE_EMIS_%Y%m%d.nc")
            )

        # pprint(self.task_config)
        # Debug output for chemistry history directory
        logger.info(f"Outputing files prescribed to {self.task_config.COMOUT_CHEM_INPUT}")
        tmpl_dict = {
            'DATA': self.task_config.DATA,
            'COMOUT_CHEM_INPUT': self.task_config.COMOUT_CHEM_INPUT,
            'AERO_EMIS_FIRE_DIR': AERO_EMIS_FIRE_DIR,
            'AERO_EMIS_FIRE_VERSION': self.task_config.AERO_EMIS_FIRE_VERSION,
            'historical': self.historical,
            'forecast_dates': self.task_config.get('forecast_dates', []),
            'qfed_vars': self.task_config.get('qfed_vars', ["co", "nox", "so2", "nh3", "bc", "oc"]),
            'gbbepx_vars': self.task_config.get('gbbepx_vars', ["co", "nox", "so2", "nh3", "bc", "oc"]),
            "rawfiles": files_found,
            "startdate": self.start_date.strftime('%Y%m%d'),
            "processed_files": processed_files,
        }

        # Parse template and update task configuration
        yaml_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'chem', 'fire_emission.yaml.j2')
        if not os.path.exists(yaml_template):
            logger.warning(f"Template file not found: {yaml_template}, using default configuration")
            yaml_config = {'fire_emission': {}}
        else:
            logger.debug(f'Parsing YAML template: {yaml_template}')
            yaml_config = parse_j2yaml(yaml_template, tmpl_dict)

        self.task_config = AttrDict(**self.task_config, **yaml_config)

        # Create working directory and sync files using FileHandler
        FileHandler(yaml_config.fire_emission.data_in).sync()

        input_files = {"rawfiles": [os.path.join(self.task_config.DATA, os.path.basename(file)) for file in files_found]}
        self.task_config = AttrDict(**self.task_config, **input_files)

    @logit(logger)
    def execute(self) -> None:
        """Process emission files based on configuration.

        For GBBEPx files, converts them to COARDS compliant format and renames
        according to template pattern.
        For QFED files, combines all data into separate files for each forecast date.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Notes
        -----
        - Uses the task_config to determine the type of emissions to process
        - For GBBEPx, it uses the GBBEPx_to_COARDS method to convert files
        - For QFED, it combines files by date using the combine_qfed_files method
        - Creates a separate output file for each date in self.forecast_dates
        - Output files are named with pattern FIRE_EMIS_YYYYMMDD.nc for each date
        - The processed files are added to the task_config for later use
        - Uses the FileHandler for file operations
        - Uses the logit decorator for logging
        - Uses decode_cf=False when processing QFED files
        """
        logger.info(f"Processing emission files in {self.task_config.DATA}")

        workdir = self.task_config.DATA

        processed_files = []

        if self.task_config.AERO_EMIS_FIRE.lower() == 'gbbepx':
            # Process GBBEPx files separately for each date
            processed_files.extend(self._process_gbbepx_files(workdir))

        elif self.task_config.AERO_EMIS_FIRE.lower() == 'qfed':
            # Process QFED files for each forecast date
            processed_files.extend(self._process_qfed_files(workdir))
        else:
            logger.warning(f"Unknown AERO_EMIS_FIRE type: {self.task_config.AERO_EMIS_FIRE}")
            raise WorkflowException(f"Unsupported AERO_EMIS_FIRE type: {self.task_config.AERO_EMIS_FIRE}")

        # Add processed files to task_config
        outdict = {'processed_files': processed_files}
        self.task_config = AttrDict(**self.task_config, **outdict)

        logger.info("Emission processing execute phase complete")

    @logit(logger)
    def finalize(self) -> None:
        """Perform closing actions of the task.
        Copy processed files from the DATA directory to COMOUT_CHEM_INPUT.

        Returns
        -------
        None

        Notes
        -----
        Only copies processed GBBEPx files or QFED files based on configuration
        Uses FileHandler for reliable file operations with logging
        """
        logger.info("Finalizing chemical emissions processing")

        FileHandler(self.task_config.fire_emission.data_out).sync()

        logger.info("Chemical emissions finalization complete")

    @logit(logger)
    def _get_unique_months(self):
        """Extract unique months and years from forecast dates.

        This method finds all unique months and years present in the forecast dates
        range. Useful for monthly-based emissions processing.

        Returns
        -------
        tuple
            A tuple containing:
                - set of unique months as zero-padded strings (01-12)
                - set of unique years as integers

        Notes
        -----
        Uses self.forecast_dates which should be populated during initialization
        Months are returned as strings with leading zeros (e.g., '01' for January)
        Years are returned as integers
        """
        months = set(f"{date.month:02d}" for date in self.forecast_dates)
        years = set(date.year for date in self.forecast_dates)
        return months, years

    @logit(logger)
    def _find_gbbepx_files(self, dates, version='v5r0'):
        """Find GBBEPx files for the given date

        Parameters
        ----------
        dates : str or list
            Date or dates for which to find GBBEPx files
        version : str
            Version of GBBEPx files to search for

        Returns
        -------
        List[str]
            List of GBBEPx files for the given date(s)
        """
        logger.info(f'Finding GBBEPx files for {dates}')

        # Find all possible months
        months, years = self._get_unique_months()

        # Format dates properly for matching
        if not isinstance(dates, list):
            dates = [dates]
        date_strings = [d.strftime('%Y%m%d') if hasattr(d, 'strftime') else str(d) for d in dates]

        files_found = []
        # Find all possible files
        for mon in months:
            try:
                emis_file_dir = os.path.join(self.task_config.AERO_EMIS_FIRE_DIR, version, mon)
                if not os.path.exists(emis_file_dir):
                    logger.warning(f"Directory does not exist: {emis_file_dir}")
                    continue

                all_files = os.listdir(emis_file_dir)

                matching_files = []

                # Look for both file patterns:
                # Pattern 1: "GBBEPx-all01GRID_v4r0_blend_s202302240000000_e202302242359590_c202302250134090.nc"
                # Pattern 2: "GBBEPx_all01GRID.emissions_v004_20150716.nc"

                for file_name in all_files:
                    match_found = False

                    # Try pattern 1 with s/e/c date format
                    pattern1 = r".*s(\d{8}).*e(\d{8}).*c(\d{8}).*\.nc"
                    match = re.match(pattern1, file_name)
                    if match:
                        start_date = match.group(1)
                        # end_date = match.group(2)
                        create_date = match.group(3)

                        # Check if the file's date matches any of our target dates
                        for date_str in date_strings:
                            # Match if the file start date is within our target dates
                            if date_str in start_date:
                                full_path = os.path.join(emis_file_dir, file_name)
                                matching_files.append(full_path)
                                logger.debug(f"Found GBBEPx file (pattern 1): {full_path}")
                                match_found = True
                                break

                    # If no match yet, try pattern 2 with YYYYMMDD format at the end
                    if not match_found and "GBBEPx" in file_name:
                        pattern2 = r".*_(\d{8})\.nc"
                        match = re.match(pattern2, file_name)
                        if match:
                            file_date = match.group(1)

                            # Check if the file's date matches any of our target dates
                            for date_str in date_strings:
                                if date_str in file_date:
                                    full_path = os.path.join(emis_file_dir, file_name)
                                    matching_files.append(full_path)
                                    logger.debug(f"Found GBBEPx file (pattern 2): {full_path}")
                                    break

                files_found.extend(matching_files)
            except (FileNotFoundError, PermissionError) as e:
                logger.warning(f"Error accessing directory {emis_file_dir}: {e}")

        return files_found

    @logit(logger)
    def _find_qfed_files(self, dates, vars, version='061', aero_emis_fire_dir=None):
        """Find QFED files for the given date(s)

        Parameters
        ----------
        dates : str, datetime, or list
            Date or dates for which to find QFED files
        vars : list
            List of variables to search for (e.g., bc, oc, co, etc.)
        version : str
            Version of QFED files to search for, will be zero-padded to 3 digits
        aero_emis_fire_dir : str, optional
            Directory containing fire emission data. If None, uses self.task_config.AERO_EMIS_FIRE_DIR

        Returns
        -------
        List[str]
            List of QFED files for the given date(s) and variables
        """
        logger.info(f'Finding QFED files for {dates}')

        # Use provided directory or fall back to config value
        logger.info(f'Using emissions directory: {aero_emis_fire_dir}')

        # ensure version is a string
        version = str(version).zfill(3)

        # Convert single date to list for consistent processing
        if not isinstance(dates, list):
            dates = [dates]

        # Format dates properly
        date_strings = [d.strftime('%Y%m%d') if hasattr(d, 'strftime') else str(d) for d in dates]

        files_found = []

        for date in dates:
            # Extract year and month from the date
            if hasattr(date, 'year') and hasattr(date, 'month'):
                year = str(date.year)
                month = f"{date.month:02d}"
            else:
                # If date is a string, try to parse it
                date_str = str(date)
                if len(date_str) >= 8:  # YYYYMMDD format
                    year = date_str[:4]
                    month = date_str[4:6]
                else:
                    logger.warning(f"Cannot parse date format: {date}")
                    continue

            emis_file_dir = os.path.join(aero_emis_fire_dir, year, month)

            if not os.path.exists(emis_file_dir):
                logger.warning(f"Directory does not exist: {emis_file_dir}")
                continue

            # Format date string for file matching
            date_str = date.strftime('%Y%m%d') if hasattr(date, 'strftime') else str(date)
            if len(date_str) > 8:  # Format may be YYYY-MM-DD
                date_str = date_str.replace('-', '')

            for v in vars:
                # Match pattern like qfed2.emis_bc.{version}.20200118.nc4
                v_pattern = f"qfed2.emis_{v}.{version}.{date_str}.nc4"
                full_path = os.path.join(emis_file_dir, v_pattern)

                # If exact match exists
                if os.path.exists(full_path):
                    files_found.append(full_path)
                    logger.debug(f"Found exact QFED file: {full_path}")

        if not files_found:
            logger.warning(f"No QFED files found for dates {date_strings} and variables {vars}")

        return files_found

    @logit(logger)
    def GBBEPx_to_COARDS(self, fname: Union[str, os.PathLike]) -> xr.Dataset:
        """Convert GBBEPx file to COARDS compliant format

        Parameters
        ----------
        fname : str | os.PathLike
            Input GBBEPx file path

        Returns
        -------
        xr.Dataset
            COARDS compliant dataset
        """
        logger.info(f"Converting {fname} to COARDS format")
        f = xr.open_dataset(fname, decode_cf=False)

        # Handle time dimension
        if 'Time' in f.dims:
            f = f.rename({"Time": 'time'})
        f.time.attrs['long_name'] = 'time'

        # Modify latitude and longitude attributes
        f = f.rename({'Longitude': 'lon', 'Latitude': 'lat'})

        # Validate and normalize coordinates
        # Check longitude range and monotonicity
        if not (f.lon.diff('lon') > 0).all():
            raise WorkflowException("Longitude values must be strictly increasing")

        # Ensure longitude is in [-180, 180] range
        f['lon'] = xr.where(f.lon > 180, f.lon - 360, f.lon)
        f = f.sortby('lon')  # Sort after potential wrapping

        # Check latitude monotonicity
        if not (f.lat.diff('lat') > 0).all():
            raise WorkflowException("Latitude values must be strictly increasing")

        f.lon.attrs.update({'long_name': 'Longitude', 'units': 'degrees_east'})
        f.lat.attrs.update({'long_name': 'Latitude', 'units': 'degrees_north'})

        # Remove Element dimension if present
        if 'Element' in f.dims:
            f = f.drop_dims('Element')

        # Update variable attributes
        for v in f.data_vars:
            if v not in ['FirePerc', 'QCAll', 'NumSensor', 'CloudPerc']:
                f[v].attrs['_FillValue'] = -9999.0
            elif v == 'FirePerc':
                f[v].attrs.update({'units': '-', 'long_name': 'percent_of_fire_in_grid_cell'})
            elif v == 'CloudPerc':
                f[v].attrs.update({'units': '-', 'long_name': 'percent_of_clouds_in_grid_cell'})
            elif v == 'NumSensor':
                f[v].attrs['units'] = '-'

        # Set global attributes
        f.attrs.update({'format': 'NetCDF', 'title': 'GBBEPx Fire Emissions'})

        return f

    @logit(logger)
    def combine_qfed_files(self, qfed_files: List[str], output_path: str = None) -> xr.Dataset:
        """Combine multiple QFED emission files into a single NetCDF file.

        Parameters
        ----------
        qfed_files : List[str]
            List of QFED file paths to combine
        output_path : str, optional
            Path where to save the combined file. If None, returns the dataset without saving.

        Returns
        -------
        xr.Dataset
            Combined dataset containing all QFED variables

        Notes
        -----
        This function loads each file individually and combines them without using dask.
        Uses decode_cf=False as required for QFED files.
        Preprocessing renames biomass variables to uppercase emission type (e.g., biomass -> BC),
        as well as related variables like biomass_tf -> BC_tf to avoid conflicts during merge.
        Files are grouped by variable type, processed, and then combined using merge with compat='override'.
        """
        if not qfed_files:
            logger.warning("No QFED files provided to combine")
            return None

        logger.info(f"Combining {len(qfed_files)} QFED files")

        try:
            # Group files by variable type for easier processing
            var_groups = {}
            for file_path in qfed_files:
                file_name = os.path.basename(file_path)
                if "qfed2.emis_" in file_name:
                    parts = file_name.split('.')
                    if len(parts) >= 3:
                        var_type = parts[1].split('_')[1].lower()  # Extract variable after emis_
                        if var_type not in var_groups:
                            var_groups[var_type] = []
                        var_groups[var_type].append(file_path)

            # Process each variable group
            datasets_by_var = {}
            for var_type, files in var_groups.items():
                logger.info(f"Processing {len(files)} files for variable: {var_type}")
                var_datasets = []

                for file_path in files:
                    logger.info(f"Opening file: {file_path}")
                    # Open dataset
                    ds = xr.open_dataset(file_path, decode_cf=False)

                    # Get uppercase variable name for this type
                    var_name = var_type.upper()

                    # Find all variables that need to be renamed for this emission type
                    rename_dict = {}
                    for dvar in ds.data_vars:
                        # Main biomass variable
                        if dvar == 'biomass':
                            rename_dict[dvar] = var_name
                        # Related variables like biomass_tf, biomass_xxx, etc.
                        elif dvar.startswith('biomass_'):
                            # Keep the suffix but prefix with the variable type
                            suffix = dvar[8:]  # Get part after 'biomass_'
                            rename_dict[dvar] = f"{var_name}_{suffix}"

                    # Rename all identified variables
                    if rename_dict:
                        logger.info(f"Renaming variables in {os.path.basename(file_path)}: {rename_dict}")
                        ds = ds.rename(rename_dict)

                    var_datasets.append(ds)

                # Concatenate datasets for this variable along the time dimension if needed
                if len(var_datasets) > 1:
                    try:
                        concat_ds = xr.concat(var_datasets, dim='time')
                        datasets_by_var[var_type] = concat_ds
                    except (ValueError, KeyError) as e:
                        logger.warning(f"Could not concatenate along time for {var_type}: {e}")
                        # If concatenation fails, just use the first dataset
                        datasets_by_var[var_type] = var_datasets[0]
                        for ds in var_datasets[1:]:
                            ds.close()
                else:
                    datasets_by_var[var_type] = var_datasets[0]

            # Merge datasets across different variables
            if datasets_by_var:
                var_list = list(datasets_by_var.values())
                combined_ds = var_list[0]

                # Merge remaining datasets with compat='override' to handle conflicting values
                for i in range(1, len(var_list)):
                    combined_ds = combined_ds.merge(var_list[i], compat='override')

                # Add global attributes
                combined_ds.attrs.update({
                    'title': 'Combined QFED emissions',
                    'source': 'QFED',
                    'created_by': 'AerosolEmissions.combine_qfed_files',
                    'creation_date': datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
                })

                # Save to file if output path is provided
                if output_path:
                    logger.info(f"Saving combined QFED dataset to {output_path}")
                    combined_ds.to_netcdf(output_path)

                # Close individual datasets to free memory
                for ds_list in datasets_by_var.values():
                    if hasattr(ds_list, 'close'):
                        ds_list.close()

                return combined_ds
            else:
                logger.warning("No valid datasets found to combine")
                return None

        except Exception as e:
            logger.error(f"Error combining QFED files: {e}")
            import traceback
            logger.error(f"Traceback: {traceback.format_exc()}")
            return None

    @logit(logger)
    def _process_gbbepx_files(self, workdir: str) -> List[str]:
        """Process GBBEPx files for each forecast date.

        Parameters
        ----------
        workdir : str
            Working directory path where processed files will be saved

        Returns
        -------
        List[str]
            List of processed file paths

        Notes
        -----
        This method processes GBBEPx files for each forecast date by:
        1. Filtering raw files by date (if date filtering logic is implemented)
        2. Converting files to COARDS format using GBBEPx_to_COARDS
        3. Saving the processed dataset to a NetCDF file
        4. Returning the list of processed file paths
        """
        logger.info(f"Processing GBBEPx files for {len(self.forecast_dates)} forecast dates")
        processed_files = []

        for forecast_date in self.forecast_dates:
            date_str = forecast_date.strftime('%Y%m%d')
            logger.info(f"Processing GBBEPx files for date {date_str}")

            # Filter files for this date - implement date filtering if needed
            date_files = []
            for file in self.task_config.rawfiles:
                # Add logic here to filter files by date if needed
                date_files.append(file)

            if date_files:
                # Process files for this date
                ds = self.GBBEPx_to_COARDS(date_files[0])  # Use the first file for this date

                # Create output filename with date
                outfile_name = f"FIRE_EMIS_{date_str}.nc"
                outfile = os.path.join(workdir, outfile_name)

                # Save the processed dataset
                comp = dict(zlib=True, complevel=2)
                encoding = {var: comp for var in ds.data_vars}
                ds.to_netcdf(outfile, encoding=encoding)
                logger.info(f"Processed emission file saved to {outfile}")

                # Add to processed files list
                processed_files.append(outfile)

                # Close dataset
                ds.close()
            else:
                logger.warning(f"No GBBEPx files found for date {date_str}")

        return processed_files

    @logit(logger)
    def _process_qfed_files(self, workdir: str) -> List[str]:
        """Process QFED files for each forecast date.

        Parameters
        ----------
        workdir : str
            Working directory path where processed files will be saved

        Returns
        -------
        List[str]
            List of processed file paths

        Notes
        -----
        This method processes QFED files for each forecast date by:
        1. Filtering raw files by date
        2. Combining files for each date using combine_qfed_files
        3. Saving the combined dataset to a NetCDF file
        4. Returning the list of processed file paths
        """
        logger.info(f"Processing QFED files for {len(self.forecast_dates)} forecast dates")
        processed_files = []

        for forecast_date in self.forecast_dates:
            date_str = forecast_date.strftime('%Y%m%d')
            logger.info(f"Processing QFED files for date {date_str}")

            # Filter files for this date
            date_files = []
            for file_path in self.task_config.rawfiles:
                file_name = os.path.basename(file_path)
                if date_str in file_name:
                    date_files.append(file_path)

            if date_files:
                logger.info(f"Found {len(date_files)} QFED files for date {date_str}")

                # Combine QFED files for this date
                ds = self.combine_qfed_files(date_files)

                if ds is not None:
                    # Create output filename with date
                    outfile_name = f"FIRE_EMIS_{date_str}.nc"
                    outfile = os.path.join(workdir, outfile_name)

                    # Save the processed dataset
                    comp = dict(zlib=True, complevel=2)
                    encoding = {var: comp for var in ds.data_vars}
                    ds.to_netcdf(outfile, encoding=encoding)
                    logger.info(f"Processed emission file for {date_str} saved to {outfile}")

                    # Add to processed files list
                    processed_files.append(outfile)

                    # Close dataset
                    ds.close()
                else:
                    logger.warning(f"Failed to combine QFED files for date {date_str}")
            else:
                logger.warning(f"No QFED files found for date {date_str}")

        return processed_files
