#!/usr/bin/env python3
"""
check_land_input_orography.py

This script compares the land mask between input surface data files and orography files for consistency across tiles.

It provides functions to:
- Count valid points in 'vtype' variable from input NetCDF files.
- Count land points in 'land_frac' variable from orography NetCDF files.
- Compare the counts between input and orography data for each tile, optionally raising errors on mismatches.

Functions
---------
count_points_from_input(input_dir: str) -> Dict

count_points_from_orog(orog_dir: str) -> Dict
    Counts the number of land points in orography data files for each tile, where 'land_frac' > 0.

compare_counts(input_counts: Dict, orog_counts: Dict, fatal: bool = False) -> None
    Compares the counts from input and orography data for each tile, logging warnings or raising errors on mismatches.

Usage
-----
Run as a script with required arguments:
    --input_dir : Path to input NetCDF files (sfc_data.tile{tile_num}.nc)
    --orog_dir  : Path to orography NetCDF files (oro_data.tile{tile_num}.nc)
    --fatal     : Optional flag to fail on first error

Example
-------
python check_land_input_orography.py --input_dir /path/to/input --orog_dir /path/to/orog --fatal
"""

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from logging import getLogger
from typing import Dict
import os
from netCDF4 import Dataset
import numpy as np
from wxflow import Logger, logit, AttrDict

logger = getLogger(__name__)
NTILES = 6  # Number of tiles expected in the input and orography data


@logit(logger)
def count_points_from_input(input_dir: str) -> Dict:
    """
    Counts the number of valid points in the 'vtype' variable from surface data NetCDF files
    in the specified input directory.
    For each tile (from 1 to NTILES), attempts to open the corresponding 'sfc_data.tile{tt}.nc' file and
    counts the number of elements greater than zero in the 'vtype' array.
    Results are stored in dictionaries keyed by tile name.

    Parameters
    ----------
    input_dir : str
        Path to the directory containing the input NetCDF files.

    Returns
    -------
    counts: Dict
        Dictionary with:
        - vtype: Number of points with 'vtype' > 0 for each tile.

    Notes
    -----
    - Logs warnings if files are missing or variables are not found.
    """

    # Initialize the counting dictionary
    counts = AttrDict()
    counts.vtype = {}

    for tt in range(1, NTILES + 1):
        try:
            fname = os.path.join(input_dir, f"sfc_data.tile{tt}.nc")
            logger.debug(f"Processing input file: {fname}")
            with Dataset(fname, mode='r') as ncid:
                counts.vtype[f"tile{tt}"] = np.sum(ncid.variables['vtype'][:] > 0)
        except FileNotFoundError:
            logger.warning(f"File {fname} not found. Skipping tile {tt}.")
            continue
        except KeyError as ee:
            logger.warning(f"Key error in file {fname}: {ee}. Skipping tile {tt}.")
            continue
        except Exception as ee:
            logger.warning(f"Unexpected error in file {fname}: {ee}. Skipping tile {tt}.")
            continue

    return counts


@logit(logger)
def count_points_from_orog(orog_dir: str) -> Dict:
    """
    Count the number of land points in orography data files for each tile.
    Parameters
    ----------
    orog_dir : str
        Directory containing orography NetCDF files named as 'oro_data.tile{tt}.nc'.
    Returns
    -------
    counts : Dict
        Dictionary with the number of land points (where 'land_frac' > 0) for each tile.
        The result is stored in `counts.land_frac` as a mapping from tile name (e.g., 'tile1') to count.
    Notes
    -----
    - The function expects NTILES to be defined globally.
    - Logs warnings if files are missing or variables are not found.
    """

    # Initialize the counting dictionary
    counts = AttrDict()
    counts.land_frac = {}

    for tt in range(1, NTILES + 1):
        try:
            fname = os.path.join(orog_dir, f"oro_data.tile{tt}.nc")
            logger.debug(f"Processing orography file: {fname}")
            with Dataset(fname, mode='r') as ncid:
                counts.land_frac[f"tile{tt}"] = np.sum(ncid.variables['land_frac'][:] > 0)
        except FileNotFoundError:
            logger.warning(f"File {fname} not found. Skipping tile {tt}.")
            continue
        except KeyError as ee:
            logger.warning(f"Key error in file {fname}: {ee}. Skipping tile {tt}.")
            continue
        except Exception as ee:
            logger.warning(f"Unexpected error in file {fname}: {ee}. Skipping tile {tt}.")
            continue

    return counts


@logit(logger)
def compare_counts(input_counts: Dict, orog_counts: Dict, fatal: bool = False) -> None:
    """
    Compare the counts from input and orography data for each tile.

    Parameters
    ----------
    input_counts : Dict
        Dictionary containing input counts with a 'vtype' attribute, which is a mapping of tile keys to counts.
    orog_counts : Dict
        Dictionary containing orography counts with a 'land_frac' attribute, which is a mapping of tile keys to counts.
    fatal : bool, optional
        If True, raise a ValueError on count mismatch; otherwise, log a warning. Default is False.

    Raises
    ------
    ValueError
        If a tile key is missing in either input_counts.vtype or orog_counts.land_frac, or if there is a count mismatch and `fatal` is True.
    """

    for tt in range(1, NTILES + 1):
        tile_key = f"tile{tt}"
        if tile_key not in input_counts.vtype or tile_key not in orog_counts.land_frac:
            logger.error(f"Tile {tile_key} missing in either input or orography counts.")
            raise ValueError(f"Tile {tile_key} missing in either input or orography counts.")

        logger.debug(f"Comparing counts for {tile_key}: input={input_counts.vtype[tile_key]}, "
                     f"orography={orog_counts.land_frac[tile_key]}")

        if input_counts.vtype[tile_key] != orog_counts.land_frac[tile_key]:
            if fatal:
                logger.error(f"Count mismatch for {tile_key}: input={input_counts.vtype[tile_key]}, "
                             f"orography={orog_counts.land_frac[tile_key]}")
                raise ValueError(f"Count mismatch for {tile_key}: input={input_counts.vtype[tile_key]}, "
                                 f"orography={orog_counts.land_frac[tile_key]}")
            else:
                logger.warning(f"Count mismatch for {tile_key}: input={input_counts.vtype[tile_key]}, "
                               f"orography={orog_counts.land_frac[tile_key]}")


if __name__ == "__main__":

    description = """
        Compare land mask between inputs and orography files for consistency
    """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--input_dir', help='full path to where the sfc_data.ttX.nc inputs are', required=True)
    parser.add_argument('--orog_dir', help='full path to where the oro_data.ttX.nc files are located', required=True)
    parser.add_argument('--fatal', action='store_true', help='fail on first error', default=False)

    args = parser.parse_args()

    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", False))

    input_counts = count_points_from_input(input_dir=args.input_dir)
    orog_counts = count_points_from_orog(orog_dir=args.orog_dir)

    compare_counts(input_counts, orog_counts)
