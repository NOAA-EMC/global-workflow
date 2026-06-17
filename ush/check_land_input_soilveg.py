#!/usr/bin/env python3
"""
check_land_input_soilveg.py

This utility validates consistency between:
  1. Input surface fields (vegetation type and soil type)
  2. Orography-derived land fraction (land_frac)

across multiple FV3 tiles.

It checks whether grid points classified as land (land_frac > 0)
have valid vegetation (vtype) and soil (stype) category values.

The script is typically used in global workflow preprocessing/QA
to detect bad or missing land surface classifications.

------------------------------------------------------------
Expected files per tile:

Input surface file:
    sfc_data.tile{tile}.nc

Orography file:
    oro_data.tile{tile}.nc

------------------------------------------------------------
Vegetation validity rules (vtype):
    Valid if:
      1–16 OR 18–20

Soil validity rules (stype):
    Valid if:
      1–13 OR 15–16

------------------------------------------------------------
Output:
    - Counts of invalid vegetation/soil points per tile
    - Total counts across all tiles
    - Warning logs for each invalid grid point
    - Optional fatal failure on first detection

------------------------------------------------------------
Usage:
    python check_land_input_soilveg.py \
        --input_dir /path/to/input \
        --orog_dir /path/to/orog \
        --fatal
"""

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from logging import getLogger
from typing import Dict
import os
from netCDF4 import Dataset
import numpy as np
from wxflow import Logger, logit, AttrDict

logger = getLogger(__name__)
NTILES = 6  # FV3 standard tile count


@logit(logger)
def compare_landfrac_soilveg(input_dir: str,
                             orog_dir: str,
                             fatal: bool = False) -> Dict:
    """
    Compare vegetation (vtype) and soil type (stype) against land fraction.

    For each tile:
      - Read land_frac from oro_data
      - Read vtype/stype from sfc_data
      - Identify land points where land_frac > 0
      - Validate vtype and stype values at those points
      - Report and optionally fail on invalid values

    Parameters
    ----------
    input_dir : str
        Directory containing sfc_data.tile{N}.nc files
    orog_dir : str
        Directory containing oro_data.tile{N}.nc files
    fatal : bool, optional
        If True, raise an exception when invalid values are found.
        If False, only log warnings. Default is False.

    Returns
    -------
    Dict (AttrDict)
        Summary containing:
          - bad_veg: dict of per-tile vegetation error counts
          - bad_soil: dict of per-tile soil error counts
          - total_bad_veg: total count with invalid vegetation type
          - total_bad_soil: total count with invalid soil type
    """

    summary = AttrDict()
    summary.bad_veg = {}
    summary.bad_soil = {}

    total_bad_veg = 0
    total_bad_soil = 0

    # Loop over FV3 tiles (1–6)
    for tile in range(1, NTILES + 1):

        sfc_file = os.path.join(input_dir, f"sfc_data.tile{tile}.nc")
        oro_file = os.path.join(orog_dir, f"oro_data.tile{tile}.nc")

        logger.info(f"Checking tile {tile}")

        # Open NetCDF files
        with Dataset(oro_file) as oro, Dataset(sfc_file) as sfc:

            land_frac = oro.variables["land_frac"][:]

            # surface fields: time dimension assumed first index
            veg_type = sfc.variables["vtype"][0, :, :]
            soil_type = sfc.variables["stype"][0, :, :]

        # Define validity masks for vtype/stype
        valid_veg = (
            ((veg_type >= 1) & (veg_type <= 16)) |
            ((veg_type >= 18) & (veg_type <= 20))
        )

        valid_soil = (
            ((soil_type >= 1) & (soil_type <= 13)) |
            ((soil_type >= 15) & (soil_type <= 16))
        )

        # Only evaluate land points
        bad_veg = (land_frac > 0) & (~valid_veg)
        bad_soil = (land_frac > 0) & (~valid_soil)

        n_bad_veg = np.count_nonzero(bad_veg)
        n_bad_soil = np.count_nonzero(bad_soil)

        total_bad_veg += n_bad_veg
        total_bad_soil += n_bad_soil

        summary.bad_veg[f"tile{tile}"] = n_bad_veg
        summary.bad_soil[f"tile{tile}"] = n_bad_soil

        logger.info(
            f"Tile {tile}: bad vegetation={n_bad_veg}, bad soil={n_bad_soil}"
        )

        # Log individual bad vegetation points
        if n_bad_veg > 0:
            j_bad, i_bad = np.where(bad_veg)
            for j, i in zip(j_bad, i_bad):
                logger.warning(
                    f"Tile {tile}: bad veg at ({j},{i}) "
                    f"land_frac={land_frac[j,i]:.3f} "
                    f"vtype={veg_type[j,i]}"
                )

        # Log individual bad soil points
        if n_bad_soil > 0:
            j_bad, i_bad = np.where(bad_soil)
            for j, i in zip(j_bad, i_bad):
                logger.warning(
                    f"Tile {tile}: bad soil at ({j},{i}) "
                    f"land_frac={land_frac[j,i]:.3f} "
                    f"stype={soil_type[j,i]}"
                )

        # Optional strict mode
        if fatal and (n_bad_veg > 0 or n_bad_soil > 0):
            raise ValueError(
                f"Tile {tile} contains invalid vegetation/soil points"
            )

    logger.info("Summary bad reports for each tile")
    for tile in range(1, NTILES + 1):
        logger.info(f"Tile {tile}: bad vegetation={summary.bad_veg[f'tile{tile}']}, "
                    f"bad soil={summary.bad_soil[f'tile{tile}']}")

    logger.info(f"Total bad vegetation points: {total_bad_veg}")
    logger.info(f"Total bad soil points: {total_bad_soil}")

    summary.total_bad_veg = total_bad_veg
    summary.total_bad_soil = total_bad_soil

    return summary


if __name__ == "__main__":

    description = ('Validate vegetation (vtype) and soil type (stype) consistency '
                   'against land fraction (land_frac) from orography files.')

    parser = ArgumentParser(description=description, formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--input_dir', help='Directory containing sfc_data.tileN.nc files', required=True)
    parser.add_argument('--orog_dir', help='Directory containing oro_data.tileN.nc files', required=True)
    parser.add_argument('--fatal', action='store_true', help='Stop execution on first invalid grid point', default=False)

    args = parser.parse_args()

    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", False))

    compare_landfrac_soilveg(input_dir=args.input_dir, orog_dir=args.orog_dir, fatal=args.fatal)
