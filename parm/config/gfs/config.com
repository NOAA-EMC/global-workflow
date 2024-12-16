# shellcheck shell=bash
# Ignore shellcheck warnings about variables not being expanded; this is what we want
# shellcheck disable=SC2016
echo "BEGIN: config.com"

# These are just templates. All templates must use single quotations so variable
#   expansion does not occur when this file is sourced. Substitution happens later
#   during runtime. It is recommended to use the helper function `declare_from_tmpl()`,
#   to do this substitution, which is defined in `ush/preamble.sh`.
#
#   Syntax for declare_from_tmpl():
#       declare_from_tmpl [-rx] $var1[:$tmpl1] [$var2[:$tmpl2]] [...]]
#
#       options:
#           -r: Make variable read-only (same as `declare -r`)
#           -x: Mark variable for declare -rx (same as `declare -x`)
#       var1, var2, etc: Variable names whose values will be generated from a template
#                   and declared
#       tmpl1, tmpl2, etc: Specify the template to use (default is "${var}_TMPL")
#
#   Examples:
#       # Current cycle and RUN
#       YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COM_ATMOS_ANALYSIS
#
#       # Previous cycle and gdas
#       RUN=${GDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl -rx \
#           COM_ATMOS_HISTORY_PREV:COM_ATMOS_HISTORY_TMPL
#
#       # Current cycle and COM for first member
#       MEMDIR='mem001' YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COM_ATMOS_HISTORY
#

#
# If any restart, input, or analysis template is updated, `setup_expt.py.fill_ROTDIR_cycled()`
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
declare -rx COM_OBS_JEDI=${COM_BASE}'/obs_jedi'

declare -rx COM_ATMOS_INPUT_TMPL=${COM_BASE}'/model/atmos/input'
declare -rx COM_ATMOS_RESTART_TMPL=${COM_BASE}'/model/atmos/restart'
declare -rx COM_ATMOS_ANALYSIS_TMPL=${COM_BASE}'/analysis/atmos'
declare -rx COM_SNOW_ANALYSIS_TMPL=${COM_BASE}'/analysis/snow'
declare -rx COM_ATMOS_HISTORY_TMPL=${COM_BASE}'/model/atmos/history'
declare -rx COM_ATMOS_MASTER_TMPL=${COM_BASE}'/model/atmos/master'
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

declare -rx COM_WAVE_RESTART_TMPL=${COM_BASE}'/model/wave/restart'
declare -rx COM_WAVE_PREP_TMPL=${COM_BASE}'/model/wave/prep'
declare -rx COM_WAVE_HISTORY_TMPL=${COM_BASE}'/model/wave/history'
declare -rx COM_WAVE_GRID_TMPL=${COM_BASE}'/products/wave/gridded'
declare -rx COM_WAVE_STATION_TMPL=${COM_BASE}'/products/wave/station'
declare -rx COM_WAVE_GEMPAK_TMPL=${COM_BASE}'/products/wave/gempak'
declare -rx COM_WAVE_WMO_TMPL=${COM_BASE}'/products/wave/wmo'

declare -rx COM_OCEAN_HISTORY_TMPL=${COM_BASE}'/model/ocean/history'
declare -rx COM_OCEAN_RESTART_TMPL=${COM_BASE}'/model/ocean/restart'
declare -rx COM_OCEAN_INPUT_TMPL=${COM_BASE}'/model/ocean/input'
declare -rx COM_OCEAN_ANALYSIS_TMPL=${COM_BASE}'/analysis/ocean'
declare -rx COM_OCEAN_LETKF_TMPL=${COM_BASE}'/analysis/ocean/letkf'
declare -rx COM_OCEAN_BMATRIX_TMPL=${COM_BASE}'/bmatrix/ocean'
declare -rx COM_OCEAN_NETCDF_TMPL=${COM_BASE}'/products/ocean/netcdf'
declare -rx COM_OCEAN_GRIB_TMPL=${COM_BASE}'/products/ocean/grib2'
declare -rx COM_OCEAN_GRIB_GRID_TMPL=${COM_OCEAN_GRIB_TMPL}'/${GRID}'

declare -rx COM_ICE_ANALYSIS_TMPL=${COM_BASE}'/analysis/ice'
declare -rx COM_ICE_LETKF_TMPL=${COM_BASE}'/analysis/ice/letkf'
declare -rx COM_ICE_BMATRIX_TMPL=${COM_BASE}'/bmatrix/ice'
declare -rx COM_ICE_INPUT_TMPL=${COM_BASE}'/model/ice/input'
declare -rx COM_ICE_HISTORY_TMPL=${COM_BASE}'/model/ice/history'
declare -rx COM_ICE_RESTART_TMPL=${COM_BASE}'/model/ice/restart'
declare -rx COM_ICE_NETCDF_TMPL=${COM_BASE}'/products/ice/netcdf'
declare -rx COM_ICE_GRIB_TMPL=${COM_BASE}'/products/ice/grib2'
declare -rx COM_ICE_GRIB_GRID_TMPL=${COM_ICE_GRIB_TMPL}'/${GRID}'

declare -rx COM_CHEM_HISTORY_TMPL=${COM_BASE}'/model/chem/history'
declare -rx COM_CHEM_ANALYSIS_TMPL=${COM_BASE}'/analysis/chem'
declare -rx COM_CHEM_BMAT_TMPL=${COM_CHEM_ANALYSIS_TMPL}'/bmatrix'

declare -rx COM_MED_RESTART_TMPL=${COM_BASE}'/model/med/restart'
