# shellcheck shell=bash
# Ignore shellcheck warnings about variables not being expanded; this is what we want
# shellcheck disable=SC2016
echo "BEGIN: config.com"

# These are just templates. All templates must use single quotations so variable
#   expansion does not occur when this file is sourced. Substitution happens later
#   during runtime. It is recommended to use the helper function `generate_com()`,
#   to do this substitution, which is defined in `ush/preamble.sh`.
#
#   Syntax for generate_com():
#       generate_com [-rx] $var1[:$tmpl1] [$var2[:$tmpl2]] [...]]
#
#       options:
#           -r: Make variable read-only (same as `decalre -r`)
#           -x: Mark variable for declare -rx (same as `declare -x`)
#       var1, var2, etc: Variable names whose values will be generated from a template
#                   and declared
#       tmpl1, tmpl2, etc: Specify the template to use (default is "${var}_TMPL")
#
#   Examples:
#       # Current cycle and RUN
#       YMD=${PDY} HH=${cyc} generate_com -rx COM_ATMOS_ANALYSIS
#
#       # Previous cycle and gdas
#       RUN=${GDUMP} YMD=${gPDY} HH=${gcyc} generate_com -rx \
#           COM_ATMOS_HISTORY_PREV:COM_ATMOS_HISTORY_TMPL
#
#       # Current cycle and COM for first member
#       MEMDIR='mem001' YMD=${PDY} HH=${cyc} generate_com -rx COM_ATMOS_HISTORY
#

#
# If any restart, input, or analysis template is updated, `setup_expt.py.fill_COMROT_cycled()`
#   must correspondingly be updated to match.
#
if [[ "${RUN_ENVIR:-emc}" == "nco" ]]; then
    COM_OBS_TMPL=$(compath.py "${envir}/obsproc/${obsproc_ver}")'/${RUN}.${YMD}/${HH}/atmos'
    COM_RTOFS_TMPL=$(compath.py "${envir}/${WAVECUR_DID}/${rtofs_ver}")
else
    COM_OBS_TMPL='${ROTDIR}/${RUN}.${YMD}/${HH}/obs'
    COM_RTOFS_TMPL='${DMPDIR}'
fi
declare -rx COM_OBS_TMPL COM_RTOFS_TMPL
declare -rx COM_OBSDMP_TMPL='${DMPDIR}/${DUMP}${DUMP_SUFFIX}.${YMD}/${HH}/atmos'

COM_BASE='${ROTDIR}/${RUN}.${YMD}/${HH}/${MEMDIR}'

declare -rx COM_TOP_TMPL='${ROTDIR}/${RUN}.${YMD}/${HH}'

declare -rx COM_CONF_TMPL=${COM_BASE}'/conf'
declare -rx COM_ATMOS_INPUT_TMPL=${COM_BASE}'/model_data/atmos/input'
declare -rx COM_ATMOS_RESTART_TMPL=${COM_BASE}'/model_data/atmos/restart'
declare -rx COM_ATMOS_ANALYSIS_TMPL=${COM_BASE}'/analysis/atmos'
declare -rx COM_LAND_ANALYSIS_TMPL=${COM_BASE}'/analysis/land'
declare -rx COM_ATMOS_HISTORY_TMPL=${COM_BASE}'/model_data/atmos/history'
declare -rx COM_ATMOS_MASTER_TMPL=${COM_BASE}'/model_data/atmos/master'
declare -rx COM_ATMOS_GRIB_TMPL=${COM_BASE}'/products/atmos/grib2'
declare -rx COM_ATMOS_GRIB_GRID_TMPL=${COM_ATMOS_GRIB_TMPL}'/${GRID}'
declare -rx COM_ATMOS_BUFR_TMPL=${COM_BASE}'/products/atmos/bufr'
declare -rx COM_ATMOS_GEMPAK_TMPL=${COM_BASE}'/products/atmos/gempak/${GRID}'
declare -rx COM_ATMOS_GENESIS_TMPL=${COM_BASE}'/products/atmos/cyclone/genesis_vital'
declare -rx COM_ATMOS_TRACK_TMPL=${COM_BASE}'/products/atmos/cyclone/tracks'
declare -rx COM_ATMOS_GOES_TMPL=${COM_BASE}'/products/atmos/goes_sim'
declare -rx COM_ATMOS_IMAGERY_TMPL=${COM_BASE}'/products/atmos/imagery'
declare -rx COM_ATMOS_OZNMON_TMPL=${COM_BASE}'/products/atmos/oznmon'
declare -rx COM_ATMOS_RADMON_TMPL=${COM_BASE}'/products/atmos/radmon'
declare -rx COM_ATMOS_MINMON_TMPL=${COM_BASE}'/products/atmos/minmon'
declare -rx COM_ATMOS_WMO_TMPL=${COM_BASE}'/products/atmos/wmo'

declare -rx COM_WAVE_RESTART_TMPL=${COM_BASE}'/model_data/wave/restart'
declare -rx COM_WAVE_PREP_TMPL=${COM_BASE}'/model_data/wave/prep'
declare -rx COM_WAVE_HISTORY_TMPL=${COM_BASE}'/model_data/wave/history'
declare -rx COM_WAVE_GRID_TMPL=${COM_BASE}'/products/wave/gridded'
declare -rx COM_WAVE_STATION_TMPL=${COM_BASE}'/products/wave/station'
declare -rx COM_WAVE_GEMPAK_TMPL=${COM_BASE}'/products/wave/gempak'
declare -rx COM_WAVE_WMO_TMPL=${COM_BASE}'/products/wave/wmo'

declare -rx COM_OCEAN_HISTORY_TMPL=${COM_BASE}'/model_data/ocean/history'
declare -rx COM_OCEAN_RESTART_TMPL=${COM_BASE}'/model_data/ocean/restart'
declare -rx COM_OCEAN_INPUT_TMPL=${COM_BASE}'/model_data/ocean/input'
declare -rx COM_OCEAN_ANALYSIS_TMPL=${COM_BASE}'/analysis/ocean'
declare -rx COM_OCEAN_2D_TMPL=${COM_BASE}'/products/ocean/2D'
declare -rx COM_OCEAN_3D_TMPL=${COM_BASE}'/products/ocean/3D'
declare -rx COM_OCEAN_XSECT_TMPL=${COM_BASE}'/products/ocean/xsect'
declare -rx COM_OCEAN_GRIB_TMPL=${COM_BASE}'/products/ocean/grib2'
declare -rx COM_OCEAN_GRIB_GRID_TMPL=${COM_OCEAN_GRIB_TMPL}'/${GRID}'

declare -rx COM_ICE_INPUT_TMPL=${COM_BASE}'/model_data/ice/input'
declare -rx COM_ICE_HISTORY_TMPL=${COM_BASE}'/model_data/ice/history'
declare -rx COM_ICE_RESTART_TMPL=${COM_BASE}'/model_data/ice/restart'

declare -rx COM_CHEM_HISTORY_TMPL=${COM_BASE}'/model_data/chem/history'
declare -rx COM_CHEM_ANALYSIS_TMPL=${COM_BASE}'/analysis/chem'

declare -rx COM_MED_RESTART_TMPL=${COM_BASE}'/model_data/med/restart'
