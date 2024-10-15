#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, Any, Union
from pprint import pformat
from glob import glob
from numpy import sort
import xarray as xr
from datetime import timedelta

from wxflow import (
    AttrDict,
    parse_j2yaml,
    FileHandler,
    Jinja,
    logit,
    Task,
    add_to_datetime,
    to_timedelta,
    WorkflowException,
    Executable,
    which,
)

logger = getLogger(__name__.split(".")[-1])


class AerosolEmissions(Task):
    """Aerosol Emissions pre-processing Task"""

    @logit(logger, name="AerosolEmissions")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Aerosol Emissions task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        current_datetime = add_to_datetime(
            self.task_config.PDY,
            to_timedelta(f"0H"),
        )
        nforecast_hours = self.task_config["FHMAX_GFS"]
        nforecast_days = nforecast_hours // 24 + 1
        forecast_dates = [current_datetime + timedelta(days=i) for i in range(nforecast_days)]

        localdict = AttrDict(
            {"cdate": current_datetime, "nforecast_days": nforecast_days, "forecast_dates": forecast_dates}
        )

        # Extend task_config with localdict
        self.task_config = AttrDict(**self.task_config, **localdict)

        # Read the aero_emission.yaml file for common configuration
        logger.info(
            f"Read the prep_emission configuration yaml file {self.task_config.PREP_EMISSION_CONFIG}"
        )
        self.task_config.aero_emission_yaml = parse_j2yaml(
            self.task_config.PREP_EMISSION_CONFIG, self.task_config
        )
        logger.debug(
            f"aero_emission_yaml:\n{pformat(self.task_config.aero_emission_yaml)}"
        )

    @staticmethod
    @logit(logger)
    def initialize(aero_emission_yaml: Dict) -> None:
        """Initialize the work directory by copying all the common fix data

        Parameters
        ----------
        upp_yaml: Dict
            Fully resolved aero_emissions.yaml dictionary
        """
        logger.info("Copy Static Data to run directory")

    @staticmethod
    @logit(logger)
    def configure(aero_emission_yaml: Dict) -> None:
        """Configure the artifacts in the work directory.
        Copy run specific data to run directory
        """
        # need to add climatology files to copy over dynamically
        logger.info(
            f"Copy '{aero_emission_yaml.aero_emissions.config.data}' data to run directory"
        )
        FileHandler(aero_emission_yaml.aero_emissions.config.data).sync()

    @staticmethod
    @logit(logger)
    def execute(workdir: Union[str, os.PathLike], aprun_cmd: str) -> None:
        """Run the executable (if any)

        Parameters
        ----------
        workdir : str | os.PathLike
            work directory with the staged data, parm files, namelists, etc.
        aprun_cmd : str
            launcher command for executable.x

        Returns
        -------
        None
        """
        AerosolEmissions.run(workdir)

    @classmethod
    @logit(logger)
    def run(cls, workdir: Union[str, os.PathLike], current_date: str = None, forecast_dates: list = None, Config_dict: Dict = {}) -> None:
        emistype = Config_dict.emistype
        ratio = Config_dict.ratio
        climfiles = sort(glob("{}{}".format(Config_dict.climfile_str, "*.nc")))
        coarsen_scale = Config_dict.coarsen_scale

        if emistype.lower() == "qfed":
            basefile = glob("qfed2.emis_*.nc4")
        elif emistype.lower() == "gbbepx":
            basefile = glob("GBBEPx_all01GRID.emissions_v*.nc")
        elif emistype.lower() == "hfed":
            basefile = glob("hfed.emis_*.x576_y361.*nc4")

        dset = AerosolEmissions.make_fire_emission(
            d=current_date,
            climos=climfiles,
            ratio=ratio,
            scale_climo=True,
            coarsen_scale=coarsen_scale,
            obsfile=basefile)

        AerosolEmissions.write_ncf(dset, Config_dict.data_out['copy'][0][0])

    @staticmethod
    @logit(logger)
    def open_qfed(fname) -> xr.Dataset:
        """
        Open QFED2 fire emissions data

        Parameters
        ----------
        fname : str or list of str
            Path(s) to the QFED2 fire emissions files

        Returns
        -------
        xr.Dataset
            Dataset containing the fire emissions data
        """

        vrs = ["BC", "CH4", "CO", "CO2", "NH3", "NOx", "OC", "PM2.5", "SO2"]
        qfed_vars = ["bc", "ch4", "co", "co2", "nh3", "no", "oc", "pm25", "so2"]

        if len(fname) > 1:
            files = sort(fname)
        else:
            files = sort(glob(fname))

        found_species = []
        dset_dict = {}
        for f in files:
            index_good = [[i, v] for i, v in enumerate(qfed_vars) if v in f]
            good = index_good[0][0]
            found_species.append(index_good[0][1])
            da = xr.open_dataset(f, decode_cf=False).biomass
            da.name = vrs[good]
            dset_dict[vrs[good]] = da

        dset = xr.Dataset(dset_dict)
        return dset

    @staticmethod
    @logit(logger)
    def open_climatology(fname) -> xr.Dataset:
        # array to house datasets
        das = []
        # print("")
        # print("Opening Climatology Files...")

        if len(fname) > 1:
            files = sort(fname)
        else:
            files = sort(glob(fname))
        # print(files)
        xr.open_dataset(files[0])
        for i, f in enumerate(files):
            # print("  opening:", f)
            das.append(xr.open_dataset(f, engine="netcdf4"))

        return xr.concat(das, dim="time")

    @staticmethod
    @logit(logger)
    def write_ncf(dset, outfile) -> None:
        """
        Write the given dataset to a NetCDF file with specified encoding.

        Parameters:
        - dset (xarray.Dataset): The dataset to be written to the NetCDF file.
        - outfile (str): The path and filename of the output NetCDF file.

        Returns:
        None
        """
        # print("Output File:", outfile)
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
        dset.load().to_netcdf(outfile, encoding=encoding)

    @staticmethod
    @logit(logger)
    def create_climatology(
        emissions, climatology, lat_coarse=50, lon_coarse=50
    ) -> xr.Dataset:
        """
        Create scaled climatology data based on emission data.

        Parameters:
        emissions (xarray.DataArray): Emission data.
        climatology (xarray.Dataset): Input climatology data.
        lat_coarse (int, optional): Coarsening factor for latitude. Defaults to 50.
        lon_coarse (int, optional): Coarsening factor for longitude. Defaults to 50.

        Returns:
        xarray.Dataset: Scaled climatology data.

        """
        # Create a copy of the climatology
        clim = climatology.copy()

        # Coarsen the climatology
        clim_coarse = climatology.coarsen(
            lat=lat_coarse, lon=lon_coarse, boundary="trim"
        ).sum()

        # Calculate the ratio of emissions to climatology and handle NaN values
        ratio = (emissions.squeeze().data / clim_coarse.where(clim_coarse > 0)).fillna(
            0
        )

        # Interpolate the ratio to match the coordinates of the climatology
        ratio_interp = ratio.sel(lat=clim.lat, lon=clim.lon, method="nearest")

        # Loop through each time slice and scale the climatology
        for index, time_slice in enumerate(clim.time):
            # Get the current time slice of the climatology
            clim_slice = clim.data[index, :, :]

            # Calculate the weighted alpha ratio parameter
            # alpha = 1.0 - 1.0 / (index + 1)

            # Scale the current time slice
            scaled_slice = clim_slice * ratio_interp[index, :, :]

            # Update the climatology with the scaled time slice
            clim.data[index, :, :] = scaled_slice.squeeze().data

        return clim.compute()

    @staticmethod
    @logit(logger)
    def make_fire_emission(
        d=None,
        climos=None,
        ratio=0.9,
        scale_climo=True,
        coarsen_scale=150,
        obsfile="GBBEPx_all01GRID.emissions_v004_20190601.nc",
    ):
        """
        Generate fire emissions data for a given date and forecast period.

        Parameters:
        - d (str or pd.Timestamp): The date for which fire emissions are generated.
        - climos (dict): Dictionary containing pre-calculated climatology data for scaling.
        - ratio (float): The ratio of original data to climatology data for blending.
        - scale_climo (bool): Flag indicating whether to scale the climatology data.
        - n_forecast_days (int): Number of forecast days.
        - obsfile (str): Path to the file containing observed fire emissions data.
        - climo_directory (str): Directory containing climatology files.

        Returns:
        - list: A list of xarray.Dataset objects representing fire emissions data for each forecast day.
        """
        # import pandas as pd
        import numpy as np

        # get the timestamp
        # dd = pd.Timestamp(d)

        # open fire emission
        if isinstance(obsfile, (str, bytes)):
            obsfile = [obsfile]
        if "QFED".lower() in obsfile[0].lower():
            g = AerosolEmissions.open_qfed(obsfile)
        else:
            g = xr.open_mfdataset(obsfile, decode_cf=False)

        # open climotology
        climo = AerosolEmissions.open_climatology(climos)
        climo = climo.sel(lat=g["lat"], lon=g["lon"], method="nearest")

        # make weighted climo
        gc = g.coarsen(lat=coarsen_scale, lon=coarsen_scale, boundary="trim").sum()

        dsets = []
        climo_scaled = {}
        for tslice in np.arange(len(climos)):
            # print(tslice)
            # make copy of original data
            if tslice == 0:
                dset = g.copy()
            else:
                dset = dsets[tslice - 1].copy()
            dset.update({"time": [float(tslice * 24)]})
            dset.time.attrs = g.time.attrs

            for v in g.data_vars:
                if not scale_climo:
                    if tslice > 5:
                        # kk = ratio * dset[v] + (1 - ratio) * climo[v].data[tslice, :, :]
                        dset[v].data = (
                            ratio * dset[v] + (1 - ratio) * climo[v].data[tslice, :, :]
                        )
                else:
                    if tslice == 0:
                        # print("creating climatology scaling for", v)
                        climo_scaled[v] = AerosolEmissions.create_climatology(
                            gc[v], climo[v], lon_coarse=150, lat_coarse=150
                        )
                    else:
                        # cn = create_climdata(gc[v],climo[v])
                        # print(cn)
                        if tslice > 5:
                            dset[v].data = (
                                ratio * dset[v] + (1 - ratio) * climo_scaled[v].data[tslice, :, :]
                            )
                        else:
                            dset[v] = dset[v]
            # print(dset)
            dsets.append(dset)
        return xr.concat(dsets, dim="time")

    @staticmethod
    @logit(logger)
    def finalize(Config_dict: Dict) -> None:
        """Perform closing actions of the task.
        Copy data back from the DATA/ directory to COM/
        """
        # print(Config_dict.data_out)
        logger.info(f"Copy '{Config_dict.data_out}' processed data to COM/ directory")
        FileHandler(Config_dict.data_out).sync()
