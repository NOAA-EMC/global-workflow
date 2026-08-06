#!/usr/bin/env python3
"""
check_land_input_soilveg.py

Validate FV3 land-surface fields against land fraction and Noah-MP
soil parameters.

For each FV3 tile, this utility checks:

1. Vegetation type (vtype) is valid at land points.
2. Soil type (stype) is valid at land points.
3. First-layer soil moisture (smc) is defined and within the
   soil-type-dependent maximum soil moisture (maxsmc).

Land points are defined as:

    land_frac > 0

Expected files per tile
-----------------------
Surface file:
    sfc_data.tile{tile}.nc

Orography file:
    oro_data.tile{tile}.nc

Vegetation validity
-------------------
Valid vtype values:

    1-16, 18-20

Soil type validity
------------------
Valid stype values:

    1-13, 15-16

Soil moisture
-------------
For soil-moisture validation, expected cells are:

    land_frac > 0
    vtype != 15
    vtype != 17

where:

    vtype = 15 : snow/ice (glacier)
    vtype = 17 : water

For expected cells, smc must:

    - be defined (not masked or NaN)
    - be greater than 0
    - be less than or equal to maxsmc

Soil type 0 is allowed for the maxsmc lookup and is assigned:

    maxsmc = 1.0

Output
------
The utility reports:

    - Invalid vegetation counts per tile
    - Invalid soil-type counts per tile
    - Invalid soil-moisture counts per tile
    - Total invalid counts across all tiles
    - Warning messages with examples of invalid points

With --fatal, the utility raises an exception when invalid
values are found.

Usage
-----
python check_land_input_soilveg.py \
    --input_dir /path/to/input \
    --orog_dir /path/to/orog \
    --soilparm_dir /path/to/noahmptable.tbl \
    --fatal
"""

import os
import re
from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser
from logging import getLogger
from typing import Dict, List, Optional

import numpy as np
from netCDF4 import Dataset
from wxflow import AttrDict, Logger, logit


logger = getLogger(__name__)

NTILES = 6

MAX_LOG_POINTS = 100
MAX_SMC_LOG_POINTS = 10


@logit(logger)
def check_land_surface_types(
    land_frac: np.ndarray,
    veg_type: np.ndarray,
    soil_type: np.ndarray,
    tile: int,
    fatal: bool = False,
) -> Dict[str, int]:
    """
    Check vegetation and soil types at land points.

    Parameters
    ----------
    land_frac : np.ndarray
        Land fraction.
    veg_type : np.ndarray
        Vegetation type.
    soil_type : np.ndarray
        Soil type.
    tile : int
        FV3 tile number.
    fatal : bool, optional
        Raise ValueError if invalid values are found.

    Returns
    -------
    Dict[str, int]
        Counts of invalid vegetation and soil points.
    """

    # ---------------------------------------------------------
    # Valid vegetation and soil types
    # ---------------------------------------------------------

    # Valid vegetation types: 1-16 and 18-20.
    valid_veg = (
        ((veg_type >= 1) & (veg_type <= 16)) |
        ((veg_type >= 18) & (veg_type <= 20))
    )

    # Valid soil types: 1-13 and 15-16.
    valid_soil = (
        ((soil_type >= 1) & (soil_type <= 13)) |
        ((soil_type >= 15) & (soil_type <= 16))
    )

    # Only land points are checked.
    land_points = land_frac > 0

    invalid_veg = land_points & ~valid_veg
    invalid_soil = land_points & ~valid_soil

    n_invalid_veg = np.count_nonzero(invalid_veg)
    n_invalid_soil = np.count_nonzero(invalid_soil)

    # ---------------------------------------------------------
    # Report invalid vegetation points
    # ---------------------------------------------------------
    if n_invalid_veg > 0:
        j_fail, i_fail = np.where(invalid_veg)

        for j, i in zip(
            j_fail[:MAX_LOG_POINTS],
            i_fail[:MAX_LOG_POINTS],
        ):
            logger.warning(
                f"Tile {tile}: invalid vegetation at ({j},{i}) "
                f"land_frac={land_frac[j, i]:.3f}, "
                f"vtype={veg_type[j, i]}"
            )

        if n_invalid_veg > MAX_LOG_POINTS:
            logger.warning(
                f"Tile {tile}: "
                f"{n_invalid_veg - MAX_LOG_POINTS} additional "
                "invalid vegetation points not shown"
            )

    # ---------------------------------------------------------
    # Report invalid soil points
    # ---------------------------------------------------------
    if n_invalid_soil > 0:
        j_fail, i_fail = np.where(invalid_soil)

        for j, i in zip(
            j_fail[:MAX_LOG_POINTS],
            i_fail[:MAX_LOG_POINTS],
        ):
            logger.warning(
                f"Tile {tile}: invalid soil type at ({j},{i}) "
                f"land_frac={land_frac[j, i]:.3f}, "
                f"stype={soil_type[j, i]}"
            )

        if n_invalid_soil > MAX_LOG_POINTS:
            logger.warning(
                f"Tile {tile}: "
                f"{n_invalid_soil - MAX_LOG_POINTS} additional "
                "invalid soil-type points not shown"
            )

    # ---------------------------------------------------------
    # Optional strict mode
    # ---------------------------------------------------------
    if fatal and (n_invalid_veg > 0 or n_invalid_soil > 0):
        raise ValueError(
            f"Tile {tile} contains invalid vegetation/soil points"
        )

    return {
        "invalid_veg": n_invalid_veg,
        "invalid_soil": n_invalid_soil,
    }


@logit(logger)
def check_soil_moisture(
    smc1: np.ndarray,
    land_frac: np.ndarray,
    veg_type: np.ndarray,
    soil_type: np.ndarray,
    porosity_table: np.ndarray,
    tile: int,
    fatal: bool = False,
) -> int:
    """
    Check first-layer soil moisture for expected land grid cells.

    Expected soil-moisture cells are defined as:

        land_frac > 0
        vtype != 15
        vtype != 17

    where:

        vtype = 15 : snow/ice (glacier)
        vtype = 17 : water

    For expected cells, soil moisture must:

        - be defined (not masked or NaN)
        - be greater than 0
        - be less than or equal to the soil-type-dependent
          maximum soil moisture (maxsmc / porosity)

    Soil type 0 is allowed and is assigned:

        maxsmc = 1.0

    Parameters
    ----------
    smc1 : np.ndarray
        First-layer soil moisture.
    land_frac : np.ndarray
        Land fraction.
    veg_type : np.ndarray
        Vegetation type.
    soil_type : np.ndarray
        Soil type.
    porosity_table : np.ndarray
        Noah-MP maxsmc lookup table.
    tile : int
        FV3 tile number.
    fatal : bool, optional
        Raise ValueError if invalid soil moisture values are found.

    Returns
    -------
    int
        Number of invalid soil-moisture points.
    """

    soil_type = np.asarray(soil_type, dtype=np.int32)

    # ---------------------------------------------------------
    # Define expected soil-moisture cells
    # ---------------------------------------------------------

    expected_smc = (
        (land_frac > 0) &
        ~np.isin(veg_type, [15, 17])
    )

    n_expected = np.count_nonzero(expected_smc)

    logger.info(
        f"Tile {tile}: expected soil moisture points: "
        f"{n_expected}"
    )

    # ---------------------------------------------------------
    # Validate soil types used for maxsmc lookup
    # ---------------------------------------------------------

    invalid_soil_type = (
        (soil_type < 0) |
        (soil_type > len(porosity_table))
    )

    if np.any(invalid_soil_type):
        invalid_values = np.unique(
            soil_type[invalid_soil_type]
        )

        raise ValueError(
            f"Tile {tile}: invalid soil_type values: "
            f"{invalid_values.tolist()}"
        )

    # ---------------------------------------------------------
    # Build soil-type-dependent maxsmc array
    # ---------------------------------------------------------

    # Default maxsmc is 1.0.
    # This handles soil_type == 0.
    maxsmc = np.ones_like(
        soil_type,
        dtype=float,
    )

    valid_lookup = (
        (soil_type > 0) &
        (soil_type <= len(porosity_table))
    )

    maxsmc[valid_lookup] = porosity_table[
        soil_type[valid_lookup] - 1
    ]

    # ---------------------------------------------------------
    # Check whether SMC is defined
    # ---------------------------------------------------------

    # Handle both regular ndarrays and masked arrays.
    smc_mask = np.ma.getmaskarray(smc1)

    # Replace masked values with NaN so that they are caught
    # by the finite-value check.
    smc_values = np.ma.filled(
        smc1,
        np.nan,
    )

    # ---------------------------------------------------------
    # Identify invalid soil-moisture points
    # ---------------------------------------------------------

    invalid_smc = (
        expected_smc &
        (
            smc_mask |
            ~np.isfinite(smc_values) |
            (smc_values <= 0.0) |
            (smc_values > maxsmc)
        )
    )

    n_invalid_smc = np.count_nonzero(invalid_smc)

    # ---------------------------------------------------------
    # Report invalid soil-moisture points
    # ---------------------------------------------------------

    if n_invalid_smc > 0:
        logger.warning(
            f"Tile {tile}: invalid soil moisture points: "
            f"{n_invalid_smc} of {n_expected} expected points"
        )

        iy, ix = np.where(invalid_smc)

        for j, i in zip(
            iy[:MAX_SMC_LOG_POINTS],
            ix[:MAX_SMC_LOG_POINTS],
        ):
            smc_value = smc_values[j, i]

            if smc_mask[j, i]:
                smc_display = "MISSING/MASKED"
            elif not np.isfinite(smc_value):
                smc_display = str(smc_value)
            else:
                smc_display = f"{smc_value:.4f}"

            logger.warning(
                f"Tile {tile}: ({j},{i}): "
                f"land_frac={land_frac[j, i]:.3f}, "
                f"vtype={veg_type[j, i]}, "
                f"soil_type={soil_type[j, i]}, "
                f"smc1={smc_display}, "
                f"smcmax={maxsmc[j, i]:.4f}"
            )

        if n_invalid_smc > MAX_SMC_LOG_POINTS:
            logger.warning(
                f"Tile {tile}: "
                f"{n_invalid_smc - MAX_SMC_LOG_POINTS} additional "
                "invalid soil moisture points not shown"
            )

    # ---------------------------------------------------------
    # Optional strict mode
    # ---------------------------------------------------------

    if fatal and n_invalid_smc > 0:
        raise ValueError(
            f"Tile {tile} contains invalid soil moisture points"
        )

    return n_invalid_smc


def _read_2d_or_3d_surface_variable(
    dataset: Dataset,
    variable_name: str,
) -> np.ndarray:
    """
    Read a surface variable stored as either 2-D or 3-D data.

    For 3-D variables, the first dimension is assumed to be the
    time dimension and index 0 is selected.

    Parameters
    ----------
    dataset : netCDF4.Dataset
        Open NetCDF dataset.
    variable_name : str
        Variable name.

    Returns
    -------
    np.ndarray
        Two-dimensional variable data.
    """

    variable = dataset.variables[variable_name]

    if variable.ndim == 3:
        return variable[0, :, :]

    return variable[:, :]


@logit(logger)
def compare_landfrac_soilveg(
    input_dir: str,
    orog_dir: str,
    soilparm_dir: str,
    fatal: bool = False,
) -> Dict:
    """
    Validate land-surface classifications and soil moisture.

    For each FV3 tile, this function:

    1. Reads land_frac from the orography file.
    2. Reads vtype, stype, and smc from the surface file.
    3. Checks vtype and stype at land points.
    4. Checks first-layer soil moisture against Noah-MP maxsmc.

    Parameters
    ----------
    input_dir : str
        Directory containing sfc_data.tile{N}.nc files.
    orog_dir : str
        Directory containing oro_data.tile{N}.nc files.
    soilparm_dir : str
        Path to the Noah-MP soil parameter table.
    fatal : bool, optional
        Raise an exception if invalid values are found.

    Returns
    -------
    Dict
        Summary containing per-tile and total invalid counts.
    """

    summary = AttrDict()

    summary.invalid_veg = {}
    summary.invalid_soil = {}
    summary.invalid_smc = {}

    total_invalid_veg = 0
    total_invalid_soil = 0
    total_invalid_smc = 0

    # ---------------------------------------------------------
    # Read Noah-MP soil parameters
    # ---------------------------------------------------------

    soilparm_file = os.path.join(soilparm_dir, 'ufs/noahmptable.tbl')

    params = read_stas_params(
        soilparm_file,
        var_list=["maxsmc"],
    )

    if "maxsmc" not in params:
        raise ValueError(
            f"maxsmc was not found in {soilparm_file}"
        )

    maxsmc_table = np.asarray(
        params["maxsmc"],
        dtype=float,
    )

    logger.info(
        f"Read {len(maxsmc_table)} maxsmc values "
        f"from {soilparm_file}"
    )

    # ---------------------------------------------------------
    # Process FV3 tiles
    # ---------------------------------------------------------

    for tile in range(1, NTILES + 1):

        sfc_file = os.path.join(
            input_dir,
            f"sfc_data.tile{tile}.nc",
        )

        oro_file = os.path.join(
            orog_dir,
            f"oro_data.tile{tile}.nc",
        )

        logger.info(f"Checking tile {tile}")

        try:
            with Dataset(oro_file) as oro, Dataset(sfc_file) as sfc:

                land_frac = oro.variables["land_frac"][:]

                veg_type = _read_2d_or_3d_surface_variable(
                    sfc,
                    "vtype",
                )

                soil_type = _read_2d_or_3d_surface_variable(
                    sfc,
                    "stype",
                )

                smc = sfc.variables["smc"]

                # Expected smc dimensions:
                #
                #     time, soil_layer, y, x
                #
                # Select the first time and first soil layer.
                smc1 = smc[0, 0, :, :]

        except FileNotFoundError:
            logger.warning(
                f"File {sfc_file} or {oro_file} not found. "
                f"Skipping tile {tile}."
            )
            continue

        except KeyError as exc:
            logger.warning(
                f"Missing variable in tile {tile}: {exc}. "
                f"Skipping tile {tile}."
            )
            continue

        except Exception as exc:
            logger.warning(
                f"Unexpected error reading tile {tile}: {exc}. "
                f"Skipping tile {tile}."
            )
            continue

        # -----------------------------------------------------
        # Check vegetation and soil types
        # -----------------------------------------------------

        surface_results = check_land_surface_types(
            land_frac=land_frac,
            veg_type=veg_type,
            soil_type=soil_type,
            tile=tile,
            fatal=fatal,
        )

        n_invalid_veg = surface_results["invalid_veg"]
        n_invalid_soil = surface_results["invalid_soil"]

        summary.invalid_veg[f"tile{tile}"] = n_invalid_veg
        summary.invalid_soil[f"tile{tile}"] = n_invalid_soil

        total_invalid_veg += n_invalid_veg
        total_invalid_soil += n_invalid_soil

        # -----------------------------------------------------
        # Check soil moisture
        # -----------------------------------------------------

        n_invalid_smc = check_soil_moisture(
            smc1=smc1,
            land_frac=land_frac,
            veg_type=veg_type,
            soil_type=soil_type,
            porosity_table=maxsmc_table,
            tile=tile,
            fatal=fatal,
        )

        summary.invalid_smc[f"tile{tile}"] = n_invalid_smc
        total_invalid_smc += n_invalid_smc

    # ---------------------------------------------------------
    # Report summary
    # ---------------------------------------------------------

    logger.info("Summary of invalid points by tile")

    for tile in range(1, NTILES + 1):
        tile_key = f"tile{tile}"

        logger.info(
            f"Tile {tile}: "
            f"invalid vegetation points="
            f"{summary.invalid_veg.get(tile_key, 0)}, "
            f"invalid soil points="
            f"{summary.invalid_soil.get(tile_key, 0)}, "
            f"invalid soil moisture points="
            f"{summary.invalid_smc.get(tile_key, 0)}"
        )

    logger.info(
        f"Total invalid vegetation points: "
        f"{total_invalid_veg}"
    )

    logger.info(
        f"Total invalid soil points: "
        f"{total_invalid_soil}"
    )

    logger.info(
        f"Total invalid soil moisture points: "
        f"{total_invalid_smc}"
    )

    summary.total_invalid_veg = total_invalid_veg
    summary.total_invalid_soil = total_invalid_soil
    summary.total_invalid_smc = total_invalid_smc

    return summary


@logit(logger)
def read_stas_params(
    file_path: str,
    var_list: Optional[List[str]] = None,
) -> Dict[str, np.ndarray]:
    """
    Read parameters from the Noah-MP soil STAS parameter block.

    The function extracts values from the:

        &noahmp_soil_stas_parameters

    block in the supplied table.

    Parameters
    ----------
    file_path : str
        Path to noahmptable.tbl.
    var_list : list[str], optional
        Variables to extract, for example:

            ["bb", "maxsmc", "satpsi"]

        If None, all variables in the block are returned.

    Returns
    -------
    Dict[str, np.ndarray]
        Dictionary mapping parameter names to NumPy arrays.
    """

    data: Dict[str, np.ndarray] = {}

    in_block = False
    current_var = None

    # Number format supporting decimal and scientific notation.
    number_pattern = (
        r"[-+]?"
        r"(?:\d+(?:\.\d*)?|\.\d+)"
        r"(?:[eE][-+]?\d+)?"
    )

    with open(file_path, "r") as file:

        for line in file:
            line = line.strip()

            # -------------------------------------------------
            # Detect block boundaries
            # -------------------------------------------------

            if line.startswith("&noahmp_soil_stas_parameters"):
                in_block = True
                continue

            if in_block and line.startswith("/"):
                break

            if not in_block:
                continue

            # Skip blank lines and comments.
            if not line or line.startswith("!"):
                continue

            # -------------------------------------------------
            # Detect a new variable
            # -------------------------------------------------

            match = re.match(
                r"^([a-zA-Z0-9_]+)\s*=",
                line,
            )

            if match:
                current_var = match.group(1).lower()

                values = re.findall(
                    number_pattern,
                    line,
                )

                if values:
                    data[current_var] = np.asarray(
                        [float(value) for value in values],
                        dtype=float,
                    )

                continue

            # -------------------------------------------------
            # Handle continuation lines
            # -------------------------------------------------

            if current_var is not None:
                values = re.findall(
                    number_pattern,
                    line,
                )

                if values:
                    data[current_var] = np.concatenate(
                        (
                            data[current_var],
                            np.asarray(
                                [float(value) for value in values],
                                dtype=float,
                            ),
                        )
                    )

    # ---------------------------------------------------------
    # Return only requested variables
    # ---------------------------------------------------------

    if var_list is not None:
        requested = {
            name.lower()
            for name in var_list
        }

        data = {
            name: values
            for name, values in data.items()
            if name in requested
        }

    return data


def main() -> None:
    """Parse command-line arguments and run the validation."""

    description = (
        "Validate vegetation (vtype), soil type (stype), and "
        "soil moisture (smc) against land fraction and "
        "Noah-MP soil parameters."
    )

    parser = ArgumentParser(
        description=description,
        formatter_class=ArgumentDefaultsHelpFormatter,
    )

    parser.add_argument(
        "--input_dir",
        required=True,
        help="Directory containing sfc_data.tileN.nc files",
    )

    parser.add_argument(
        "--orog_dir",
        required=True,
        help="Directory containing oro_data.tileN.nc files",
    )

    parser.add_argument(
        "--soilparm_dir",
        required=True,
        help="Path to the Noah-MP soil parameter table",
    )

    parser.add_argument(
        "--fatal",
        action="store_true",
        default=False,
        help=(
            "Exit with an error if invalid grid points "
            "are found"
        ),
    )

    args = parser.parse_args()

    # Configure wxflow logger after parsing command-line arguments.
    global logger

    logger = Logger(
        logfile_path=os.environ.get("LOGFILE_PATH"),
        level=os.environ.get("LOGGING_LEVEL", "INFO"),
        colored_log=os.environ.get("COLORED_LOG", False),
    )

    compare_landfrac_soilveg(
        input_dir=args.input_dir,
        orog_dir=args.orog_dir,
        soilparm_dir=args.soilparm_dir,
        fatal=args.fatal,
    )


if __name__ == "__main__":
    main()
