# shellcheck shell=bash
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
#           -x: Mark variable for export (same as `declare -x`)
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

# Ignore shellcheck warnings about variables not being expanded; this is what we want
# shellcheck disable=SC2016
export COM_OBS_TMPL='${ROTDIR}/${RUN}.${YMD}/${HH}/obs'
export COM_OBSDMP_TMPL='${DMPDIR}/${DUMP}${DUMP_SUFFIX}.${YMD}/${HH}/atmos'

COM_BASE='${ROTDIR}/${RUN}.${YMD}/${HH}/${MEMDIR}'

export COM_TOP_TMPL='${ROTDIR}/${RUN}.${YMD}/${HH}'

export COM_ATMOS_INPUT_TMPL=${COM_BASE}'/atmos/model_data/input'
export COM_ATMOS_RESTART_TMPL=${COM_BASE}'/atmos/model_data/restart'
export COM_ATMOS_ANALYSIS_TMPL=${COM_BASE}'/atmos/model_data/analysis'
export COM_ATMOS_HISTORY_TMPL=${COM_BASE}'/atmos/model_data/history'
export COM_ATMOS_MASTER_TMPL=${COM_BASE}'/atmos/model_data/master'
export COM_ATMOS_GRIB_TMPL=${COM_BASE}'/atmos/products/${RES}'
export COM_ATMOS_BUFR_TMPL=${COM_BASE}'/atmos/products/bufr'
export COM_ATMOS_GEMPAK_TMPL=${COM_BASE}'/atmos/products/gempak'
export COM_ATMOS_GENESIS_TMPL=${COM_BASE}'/atmos/products/cyclone/genesis_vital'
export COM_ATMOS_TRACK_TMPL=${COM_BASE}'/atmos/products/cyclone/tracks'
export COM_ATMOS_GOES_TMPL=${COM_BASE}'/atmos/products/goes_sim'
export COM_ATMOS_IMAGERY_TMPL=${COM_BASE}'/atmos/products/imagery'
export COM_ATMOS_MINMON_TMPL=${COM_BASE}'/atmos/products/minmon'
export COM_ATMOS_WAFS_TMPL=${COM_BASE}'/atmos/products/wafs'
export COM_ATMOS_WMO_TMPL=${COM_BASE}'/atmos/products/wmo'

export COM_WAVE_RESTART_TMPL=${COM_BASE}'/wave/model_data/restart'
export COM_WAVE_PREP_TMPL=${COM_BASE}'/wave/model_data/prep'
export COM_WAVE_HISTORY_TMPL=${COM_BASE}'/wave/model_data/history'
export COM_WAVE_GRID_TMPL=${COM_BASE}'/wave/products/gridded'
export COM_WAVE_STATION_TMPL=${COM_BASE}'/wave/products/station'

export COM_OCEAN_HISTORY_TMPL=${COM_BASE}'/ocean/model_data/history'
export COM_OCEAN_RESTART_TMPL=${COM_BASE}'/ocean/model_data/restart'
export COM_OCEAN_INPUT_TMPL=${COM_BASE}'/ocean/model_data/input'
export COM_OCEAN_ANALYSIS_TMPL=${COM_BASE}'/ocean/model_data/analysis'
export COM_OCEAN_2D_TMPL=${COM_BASE}'/ocean/products/2D'
export COM_OCEAN_3D_TMPL=${COM_BASE}'/ocean/products/3D'
export COM_OCEAN_DAILY_TMPL=${COM_BASE}'/ocean/products/daily'
export COM_OCEAN_XSECT_TMPL=${COM_BASE}'/ocean/products/xsect'
export COM_OCEAN_GRIB_TMPL=${COM_BASE}'/ocean/products/${RES}'

export COM_ICE_INPUT_TMPL=${COM_BASE}'/ice/model_data/input'
export COM_ICE_HISTORY_TMPL=${COM_BASE}'/ice/model_data/history'
export COM_ICE_RESTART_TMPL=${COM_BASE}'/ice/model_data/restart'

export COM_CHEM_HISTORY_TMPL=${COM_BASE}'/chem/model_data/history'

export COM_MED_RESTART_TMPL=${COM_BASE}'/med/model_data/restart'
