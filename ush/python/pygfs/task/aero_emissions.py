#!/usr/bin/env python3

import os
from logging import getLogger
from pprint import pformat
from typing import Any, Dict, List

import xarray as xr
from dateutil.rrule import DAILY, rrule
from wxflow import (
    AttrDict,
    FileHandler,
    Task,
    logit,
    parse_j2yaml,
    to_timedelta,
)

logger = getLogger(__name__.split(".")[-1])


class AerosolEmissions(Task):
    """Aerosol Emissions pre-processing Task"""

    @logit(logger, name="AerosolEmissions")
    def __init__(self, config: Dict[str, Any]) -> None:
        """
        Constructor for the Aerosol Emissions task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        nforecast_hours = self.task_config["FHMAX_GFS"]
        blend_start_date = self.task_config["PDY"]
        blend_end_date = blend_start_date + to_timedelta(f'{nforecast_hours + 24}H')
        forecast_dates = list(rrule(freq=DAILY, dtstart=blend_start_date, until=blend_end_date))

        # add forecast_dates to the task_config for parsing yaml file
        localdict = AttrDict({"forecast_dates": forecast_dates})
        self.task_config = AttrDict(**self.task_config, **localdict)

        # populate yaml file and add to task_config
        logger.info(f"Read the prep_emission configuration yaml file {self.task_config.PREP_EMISSION_CONFIG}")
        self.task_config.aero_emission_yaml = parse_j2yaml(self.task_config.PREP_EMISSION_CONFIG, self.task_config)
        logger.debug(f"aero_emission_yaml:\n{pformat(self.task_config.aero_emission_yaml)}")

        config = self.task_config.aero_emission_yaml['aero_emissions']['config']
        qfedfiles = [os.path.basename(fname[0]) for fname in config['data_in']['qfed']['copy']]
        hfedfiles = [os.path.basename(fname[0]) for fname in config['data_in']['hfed']['copy']]
        gbbepxfiles = [os.path.basename(fname[0]) for fname in config['data_in']['gbbepx']['copy']]
        climofiles = [os.path.basename(fname[0]) for fname in config['data_in']['climo']['copy']]
        n_persist = config['n_persist']

        localdict = AttrDict(
            {
                "cdate": blend_start_date,
                "nforecast_days": nforecast_hours // 24,
                "workdir": self.task_config.DATA,
                "current_date": self.task_config.PDY,
                'config': config,
                'emistype': config['emistype'],
                'climofiles': climofiles,
                'qfedfiles': qfedfiles,
                'hfedfiles': hfedfiles,
                'gbbepxfiles': gbbepxfiles,
                'n_persist': n_persist
            }
        )

        # TODO: if AERO_EMIS_FIRE is not 'blended' we don't want to do anything!

        # Extend task_config with localdict
        self.task_config = AttrDict(**self.task_config, **localdict)

    @logit(logger)
    def initialize(self) -> None:
        """
        Initialize the work directory by copying all the common fix data

        Parameters
        ----------
        None

        Returns
        -------
        None
        """
        logger.info("Copy Static Data to run directory")

        data_in = self.task_config.config.data_in
        emistype = self.task_config.emistype

        # Copy climatology files to run directory except for HFED
        # HFED is already smoothed, monthly data
        # QFED or GBBBEPx will be blended with climatology
        if emistype.lower() != 'hfed':
            logger.info("Copy climatology data to run directory")
            FileHandler(data_in.climo).sync()
            logger.info(f"Copy {emistype} data to run directory")
            FileHandler(data_in[emistype.lower()]).sync()

    @logit(logger)
    def run(self) -> None:
        """
        Run the AerosolEmissions task with the given parameters.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """
        config_dict = self.task_config['config']
        emistype = self.task_config['emistype']
        ratio = config_dict['ratio']
        climfiles = self.task_config['climofiles']
        coarsen_scale = config_dict['coarsen_scale']
        out_var_dict = config_dict['output_var_map']
        n_persist = config_dict['n_persist']

        emission_map = {'qfed': self.task_config['qfedfiles'],
                        'gbbepx': self.task_config['gbbepxfiles'],
                        'hfed': self.task_config['hfedfiles']}

        try:
            basefile = emission_map[emistype.lower()]
        except KeyError as e:
            logger.exception(f"{emistype.lower()} is not a supported emission type")
            raise Exception(
                f"FATAL ERROR: {emistype.lower()} is not a supported emission type, ABORT!"
            ) from e

        if emistype.lower() != 'hfed':
            dset = AerosolEmissions.make_fire_emission(
                climos=climfiles,
                ratio=ratio,
                scale_climo=True,
                coarsen_scale=coarsen_scale,
                obsfile=basefile,
                out_var_dict=out_var_dict,
                n_persist=n_persist)

            AerosolEmissions.write_ncf(dset, config_dict.data_out['copy'][0][0])

    @staticmethod
    @logit(logger)
    def open_qfed(files: List[str], out_var_dict: Dict[str, str] = None) -> xr.Dataset:
        """
        Open QFED2 fire emissions data combining files into one Dataset
        and renaming variables to standard (GBBEPx) names.

        Parameters
        ----------
        files : list
            Paths to the QFED2 fire emissions files
        out_var_dict : dict
            Mapping of input variable name to desired (output) variable name.

        Returns
        -------
        xr.Dataset
            Dataset containing the fire emissions data
        """
        if out_var_dict is None:
            logger.info("No output variable mapping provided")
            raise Exception("FATAL ERROR: No output variable mapping provided")

        if len(files) == 0:
            logger.info("No files provided")
            raise Exception("FATAL ERROR: Received empty list of QFED files")

        found_species = []
        dset_dict = {}
        for f in sorted(files):
            logger.info(f"Opening QFED file: {f}")
            _, input_var = os.path.basename(f).split(".")[1].split("_")
            found_species.append(input_var)
            try:
                with xr.open_dataset(f, decode_cf=False).biomass as da:
                    da.name = out_var_dict[input_var]
                    dset_dict[da.name] = da
            except Exception as e:
                logger.exception(f"Unable to read dataset: {f}")
                raise Exception("FATAL ERROR: Unable to read dataset, ABORT!") from e

        dset = xr.Dataset(dset_dict)

        return dset

    @staticmethod
    @logit(logger)
    def open_climatology(files: List[str]) -> xr.Dataset:
        """
        Open climatology files and concatenate them along the time dimension.

        Parameters
        ----------
        files : list
            Paths to the climatology files.

        Returns
        -------
        xr.Dataset
            Concatenated dataset containing the climatology data.
        """
        das = []

        logger.info("Process Climatology Files")
        for f in sorted(files):
            logger.info(f"Opening Climatology File: {f}")
            try:
                with xr.open_dataset(f, engine="netcdf4") as da:
                    das.append(da)
            except Exception as e:
                logger.exception(f"Encountered an error reading climatology file: {f}")
                raise Exception("FATAL ERROR: Unable to read file, ABORT!") from e

        return xr.concat(das, dim="time")

    @staticmethod
    @logit(logger)
    def write_ncf(dset: xr.Dataset, outfile: str) -> None:
        """
        Write the given dataset to a NetCDF file with specified encoding.

        Parameters
        ----------
        dset : xarray.Dataset
            The dataset to be written to the NetCDF file.
        outfile : str
            The path and filename of the output NetCDF file.

        Returns
        -------
        None
        """
        encoding = {}
        for v in dset.data_vars:
            encoding[v] = dict(zlib=True, complevel=4)
        if "latitude" in dset:
            encoding["latitude"] = dict(zlib=True, complevel=4)
            encoding["longitude"] = dict(zlib=True, complevel=4)
        if "lat_b" in dset:
            encoding["lat_b"] = dict(zlib=True, complevel=4)
            encoding["lon_b"] = dict(zlib=True, complevel=4)
        if "time" in dset:
            encoding["time"] = dict(dtype="i4")
        try:
            dset.load().to_netcdf(outfile, encoding=encoding)
        except Exception as e:
            logger.exception("Encountered an error writing dataset")
            raise Exception("FATAL ERROR: Unable to write dataset, ABORT!") from e

    @staticmethod
    @logit(logger)
    def create_climatology(
        emissions: xr.DataArray, climatology: xr.DataArray, lat_coarse: int = 50, lon_coarse: int = 50
    ) -> xr.Dataset:
        """
        Create scaled daily climatology data based on observed emission data.

        Parameters
        ----------
        emissions : xarray.DataArray
            Emission data. Just one time step. Same grid as the climatology.
        climatology :  xarray.Dataset
            Input climatology data. Multiple days of daily data.
        lat_coarse : int, optional
            Coarsening factor for latitude. Defaults to 50 (0.1 deg -> 5 deg).
        lon_coarse : int, optional
            Coarsening factor for longitude. Defaults to 50 (0.1 deg -> 5 deg).

        Returns
        -------
        xarray.Dataset
            Scaled climatology data.
        """
        # Create a copy of the climatology
        clim = climatology.copy()

        # We coarsen for regional scaling, to avoid small differences in fire locations
        coarsen_kws = dict(lat=lat_coarse, lon=lon_coarse, boundary="trim")
        clim_coarse = climatology.coarsen(**coarsen_kws).sum()
        obs_coarse = emissions.squeeze().coarsen(**coarsen_kws).sum()

        # Calculate the coarse ratio of emissions to climatology
        # Where climatology is not positive, the ratio will be 0
        ratio_coarse = (obs_coarse.data / clim_coarse.where(clim_coarse > 0)).fillna(0)

        # Interpolate (uncoarsen) the ratio to match the coordinates of the climatology
        # (this should be the same grid as the QFED/GBBEPx emissions)
        ratio = ratio_coarse.sel(lat=clim.lat, lon=clim.lon, method="nearest")

        # Scale data
        clim.data = clim.data * ratio.data

        return clim.compute()

    @staticmethod
    @logit(logger)
    def make_fire_emission(
        climos: List[str],
        ratio: float,
        scale_climo: bool,
        coarsen_scale: int,
        obsfile: str,
        out_var_dict: Dict[str, str],
        n_persist: int,
    ) -> xr.Dataset:
        """
        Generate fire emissions data for a given date and forecast period.

        Parameters
        ----------
        climos : list
            List of pre-calculated climatology data files for scaling.
        ratio : float
            The ratio of original data to climatology data for blending.
        scale_climo : bool
            Flag indicating whether to scale the climatology data.
        n_forecast_days : int
            Number of forecast days.
        obsfile : str
            Path to the file containing observed fire emissions data.
        climo_directory : str
            Directory containing climatology files.
        n_persist : int
            Assumed number of days that are able to be persistent fire emissions

        Returns
        -------
        xr.Dataset
            xarray Dataset object representing fire emissions data for each forecast day.
        """
        # Open fire emissions
        if isinstance(obsfile, (str, bytes)):
            obsfile = [obsfile]
        if "qfed" in obsfile[0].lower():
            obs = AerosolEmissions.open_qfed(obsfile, out_var_dict=out_var_dict)
        else:
            # GBBEPx, already combined and with correct names
            obs = xr.open_mfdataset(obsfile, decode_cf=False)

        # Open climatology
        climo = AerosolEmissions.open_climatology(climos)
        climo = climo.sel(lat=obs["lat"], lon=obs["lon"], method="nearest")

        # Blend
        dsets = []
        climo_scaled = {}
        for tslice in range(len(climos)):
            if tslice == 0:
                dset = obs.copy()
            else:
                dset = dsets[tslice - 1].copy()
            dset.update({"time": [float(tslice * 24)]})
            dset.time.attrs = obs.time.attrs

            for v in obs.data_vars:
                if not scale_climo:
                    if tslice > n_persist:
                        dset[v].data = (
                            ratio * dset[v] + (1 - ratio) * climo[v].data[tslice, :, :]
                        )
                else:
                    if tslice == 0:
                        climo_scaled[v] = AerosolEmissions.create_climatology(
                            obs[v], climo[v], lon_coarse=coarsen_scale, lat_coarse=coarsen_scale
                        )
                    else:
                        if tslice > n_persist:
                            dset[v].data = (
                                ratio * dset[v] + (1 - ratio) * climo_scaled[v].data[tslice, :, :]
                            )

            dsets.append(dset)

        return xr.concat(dsets, dim="time")

    @logit(logger)
    def finalize(self) -> None:
        """
        Perform closing actions of the task.
        Copy data back from the DATA/ directory to COM/

        Parameters
        ----------
        None

        Returns
        -------
        None
        """
        logger.info(f"Copy '{self.task_config.config.data_out}' processed data to COM/ directory")
        FileHandler(self.task_config.config.data_out).sync()
