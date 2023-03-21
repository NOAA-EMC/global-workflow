# shellcheck shell=bash
echo "BEGIN: config.com"

# These are just templates. Substitute variables in at runtime like this:
#   ```
#   COM_OBS=$(echo "${COM_OBS_TMPL}" | envsubst)
#   ```
#
# Or use the `generate_com` function included below, which uses the same
#   options as `declare`/`typeset`
#
#   # Assign COM_OBS from COM_OBS_TMPL template and mark it for export and read-only
#   generate_com -rx COM_OBS
#
# If you need to override variables in the template without changing the
#   actual variable in your program, you can make assignments in the subshell:
#
#   COM_ATMOS_RESTART_PREV=$({
#       # Override env variables for this subshell to get correct template substitution
#      RUN=${rCDUMP}
#      PDY="${PDY_PREV}"
#      cyc="${cyc_PREV}"
#      echo "${COM_ATMOS_RESTART_TMPL}" | envsubst
#    })
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
