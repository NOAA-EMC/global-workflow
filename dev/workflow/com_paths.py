#!/usr/bin/env python3

"""
Single source of truth for COM_*_TMPL path templates.

Replaces the previous ``parm/config/{app}/config.com`` shell file. Templates are
literal strings using shell-style ``${VAR}`` placeholders; substitution is done
later (by Rocoto ``<cyclestr>`` or by ``declare_from_tmpl`` at job runtime).
"""

from typing import Dict

__all__ = ['get_com_templates', 'COM_BASE', 'COM_TOP']

# Base path containing the per-member COM tree.
COM_BASE = '${ROTDIR}/${RUN}.${YMD}/${HH}/${MEMDIR}'

# Top-level cycle directory (no member component).
COM_TOP = '${ROTDIR}/${RUN}.${YMD}/${HH}'

# Fixed sub-paths under COM_BASE. Key becomes ``COM_<key>_TMPL``.
_COM_BASE_SUBPATHS: Dict[str, str] = {
    'CONF': 'conf',

    'ATMOS_INPUT': 'model/atmos/input',
    'ATMOS_RESTART': 'model/atmos/restart',
    'ATMOS_ANALYSIS': 'analysis/atmos',
    'SNOW_ANALYSIS': 'analysis/snow',
    'SNOW_ANLMON': 'products/snow/anlmon',
    'ATMOS_HISTORY': 'model/atmos/history',
    'ATMOS_MASTER': 'model/atmos/master',
    'ATMOS_GRIB': 'products/atmos/grib2',
    'ATMOS_GRIB_GRID': 'products/atmos/grib2/${GRID}',
    'ATMOS_BUFR': 'products/atmos/bufr',
    'ATMOS_GEMPAK': 'products/atmos/gempak/${GRID}',
    'ATMOS_GENESIS': 'products/atmos/cyclone/genesis_vital',
    'ATMOS_TRACK': 'products/atmos/cyclone/tracks',
    'ATMOS_GOES': 'products/atmos/goes_sim',
    'ATMOS_IMAGERY': 'products/atmos/imagery',
    'ATMOS_OZNMON': 'products/atmos/oznmon',
    'ATMOS_RADMON': 'products/atmos/radmon',
    'ATMOS_MINMON': 'products/atmos/minmon',
    'ATMOS_ANLMON': 'products/atmos/anlmon',
    'ATMOS_WMO': 'products/atmos/wmo',

    'WAVE_RESTART': 'model/wave/restart',
    'WAVE_INIT': 'model/wave/init',
    'WAVE_HISTORY': 'model/wave/history',
    'WAVE_GRID': 'products/wave/gridded',
    'WAVE_GRID_RES': 'products/wave/gridded/${GRDRESNAME}',
    'WAVE_STATION': 'products/wave/station',
    'WAVE_GEMPAK': 'products/wave/gempak',
    'WAVE_WMO': 'products/wave/wmo',

    'OCEAN_HISTORY': 'model/ocean/history',
    'OCEAN_RESTART': 'model/ocean/restart',
    'OCEAN_INPUT': 'model/ocean/input',
    'OCEAN_ANALYSIS': 'analysis/ocean',
    'OCEAN_ANLMON': 'products/ocean/anlmon',
    'OCEAN_LETKF': 'analysis/ocean/letkf',
    'OCEAN_BMATRIX': 'bmatrix/ocean',
    'OCEAN_NETCDF': 'products/ocean/netcdf',
    'OCEAN_GRIB': 'products/ocean/grib2',
    'OCEAN_GRIB_GRID': 'products/ocean/grib2/${GRID}',

    'ICE_ANALYSIS': 'analysis/ice',
    'ICE_LETKF': 'analysis/ice/letkf',
    'ICE_ANLMON': 'products/ice/anlmon',
    'ICE_BMATRIX': 'bmatrix/ice',
    'ICE_INPUT': 'model/ice/input',
    'ICE_HISTORY': 'model/ice/history',
    'ICE_RESTART': 'model/ice/restart',
    'ICE_NETCDF': 'products/ice/netcdf',
    'ICE_GRIB': 'products/ice/grib2',
    'ICE_GRIB_GRID': 'products/ice/grib2/${GRID}',

    'CHEM_HISTORY': 'model/chem/history',
    'CHEM_ANALYSIS': 'analysis/chem',
    'CHEM_BMAT': 'analysis/chem/bmatrix',
    'CHEM_ANLMON': 'products/chem/anlmon',
    'CHEM_INPUT': 'model/chem/input',
    'CHEM_RESTART': 'model/chem/restart',

    'MED_RESTART': 'model/med/restart',
}


def get_com_templates() -> Dict[str, str]:
    """
    Return all COM_*_TMPL keys with their literal template strings.

    The NCO/EMC-conditional templates (``COM_OBSPROC_TMPL`` and
    ``COM_OBSFORGE_TMPL``) are defined in ``config.base`` because they need
    shell-time evaluation of ``compath.py`` under the NCO branch.

    Returns
    -------
    Dict[str, str]
        Mapping ``COM_<NAME>_TMPL -> '${ROTDIR}/.../<subpath>'``.
    """
    tmpls: Dict[str, str] = {
        'COM_TOP_TMPL': COM_TOP,
        'COM_OBS_TMPL': f'{COM_TOP}/obs',
    }
    for name, sub in _COM_BASE_SUBPATHS.items():
        tmpls[f'COM_{name}_TMPL'] = f'{COM_BASE}/{sub}'
    return tmpls
