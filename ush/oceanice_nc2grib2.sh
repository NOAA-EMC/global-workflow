#!/bin/bash

# This script contains functions to convert ocean/ice rectilinear netCDF files to grib2 format
# This script uses the wgrib2 utility to convert the netCDF files to grib2 format and then indexes it

source "${USHgfs}/preamble.sh"

################################################################################
function _ice_nc2grib2 {
# This function converts the ice rectilinear netCDF files to grib2 format

  # Set the inputs
  local grid=${1} # 0p25, 0p50, 1p00, 5p00
  local latlon_dims=${2} # 0:721:0:1440, 0:361:0:720, 0:181:0:360, 0:36:0:72
  local current_cycle=${3} # YYYYMMDDHH
  local aperiod=${4} # 0-6
  local infile=${5} # ice.0p25.nc
  local outfile=${6} # ice.0p25.grib2
  local template=${7} # template.global.0p25.gb2

  ${WGRIB2} "${template}" \
  -import_netcdf "${infile}" "hi_h" "0:1:${latlon_dims}" \
      -set_var ICETK -set center 7 \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "aice_h" "0:1:${latlon_dims}" \
      -set_var ICEC -set center 7 \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "Tsfc_h" "0:1:${latlon_dims}" \
      -set_var ICETMP -set center 7 -rpn "273.15:+" \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "uvel_h" "0:1:${latlon_dims}" \
      -set_var UICE -set center 7 \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "vvel_h" "0:1:${latlon_dims}" \
      -set_var VICE -set center 7 \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out "${outfile}"

# Additional variables needed for GFSv17/GEFSv13 operational forecast
# files, but GRIB2 parameters not available in NCEP (-set center 7)
# tables in wgrib2 v2.0.8:

#  -import_netcdf "${infile}" "hs_h" "0:1:${latlon_dims}" \
#    -set_var ??? -set center 7 \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
#  -import_netcdf "${infile}" "frzmlt_h" "0:1:${latlon_dims}" \
#    -set_var ??? -set center 7 \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
#  -import_netcdf "${infile}" "albsni_h" "0:1:${latlon_dims}" \
#    -set_var ALBICE -set center 7 -rpn "100.0:/" \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
#  -import_netcdf "${infile}" "mlt_onset_h" "0:1:${latlon_dims}" \
#    -set_var ??? -set center 7 \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
#  -import_netcdf "${infile}" "frz_onset_h" "0:1:${latlon_dims}" \
#    -set_var ??? -set center 7 \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}"

  rc=$?
  # Check if the conversion was successful
  if (( rc != 0 )); then
   echo "FATAL ERROR: Failed to convert the ice rectilinear netCDF file to grib2 format"
  fi
  return "${rc}"

}

################################################################################
function _ocean2D_nc2grib2 {
# This function converts the ocean 2D rectilinear netCDF files to grib2 format

  # Set the inputs
  local grid=${1} # 0p25, 0p50, 1p00, 5p00
  local latlon_dims=${2} # 0:721:0:1440, 0:361:0:720, 0:181:0:360, 0:36:0:72
  local current_cycle=${3} # YYYYMMDDHH
  local aperiod=${4} # 0-6
  local infile=${5} # ocean.0p25.nc
  local outfile=${6} # ocean_2D.0p25.grib2
  local template=${7} # template.global.0p25.gb2

  ${WGRIB2} "${template}" \
  -import_netcdf "${infile}" "SSH" "0:1:${latlon_dims}" \
    -set_var SSHG -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "SST" "0:1:${latlon_dims}" \
    -set_var WTMP -set center 7 -rpn "273.15:+" \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "SSS" "0:1:${latlon_dims}" \
    -set_var SALIN -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "speed" "0:1:${latlon_dims}" \
    -set_var SPC -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "SSU" "0:1:${latlon_dims}" \
    -set_var UOGRD -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "SSV" "0:1:${latlon_dims}" \
    -set_var VOGRD -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "latent" "0:1:${latlon_dims}" \
    -set_var LHTFL -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "sensible" "0:1:${latlon_dims}" \
    -set_var SHTFL -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "SW" "0:1:${latlon_dims}" \
    -set_var DSWRF -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "LW" "0:1:${latlon_dims}" \
    -set_var DLWRF -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "LwLatSens" "0:1:${latlon_dims}" \
    -set_var THFLX -set center 7 \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
  -import_netcdf "${infile}" "MLD_003" "0:1:${latlon_dims}" \
    -set_var WDEPTH -set center 7 -set_lev "mixed layer depth" \
    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}"

# Additional variables needed for GFSv17/GEFSv13 operational forecast
# files, but GRIB2 parameters not available in NCEP (-set center 7)
# tables in wgrib2 v2.0.8:
#
#  -import_netcdf "${infile}" "Heat_PmE" "0:1:${latlon_dims}" \
#    -set_var DWHFLUX -set center 7 \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
#  -import_netcdf "${infile}" "taux" "0:1:${latlon_dims}" \
#    -set_var XCOMPSS -set center 7 \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}" \
#  -import_netcdf "${infile}" "tauy" "0:1:${latlon_dims}" \
#    -set_var YCOMPSS -set center 7 \
#    -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
#    -set_scaling same same -set_grib_type c1 -grib_out "${outfile}"

  rc=$?
  # Check if the conversion was successful
  if (( rc != 0 )); then
   echo "FATAL ERROR: Failed to convert the ocean rectilinear netCDF file to grib2 format"
  fi
  return "${rc}"

}

################################################################################
function _ocean3D_nc2grib2 {
# This function converts the ocean 3D rectilinear netCDF files to grib2 format

  # Set the inputs
  local grid=${1} # 0p25, 0p50, 1p00, 5p00
  local latlon_dims=${2} # 0:721:0:1440, 0:361:0:720, 0:181:0:360, 0:36:0:72
  local levels=${3} # 5:15:25:35:45:55:65:75:85:95:105:115:125
  local current_cycle=${4} # YYYYMMDDHH
  local aperiod=${5} # 0-6
  local infile=${6} # ocean.0p25.nc
  local outfile=${7} # ocean_3D.0p25.grib2
  local template=${8} # template.global.0p25.gb2

  IFS=':' read -ra depths <<< "${levels}"

  zl=0
  for depth in "${depths[@]}"; do

    [[ -f "tmp.gb2" ]] && rm -f "tmp.gb2"

    ${WGRIB2} "${template}" \
    -import_netcdf "${infile}" "temp" "0:1:${zl}:1:${latlon_dims}" \
      -set_var WTMP -set center 7 -rpn "273.15:+" \
      -set_lev "${depth} m below water surface" \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
    -import_netcdf "${infile}" "so" "0:1:${zl}:1:${latlon_dims}" \
      -set_var SALIN -set center 7 \
      -set_lev "${depth} m below water surface" \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
    -import_netcdf "${infile}" "uo" "0:1:${zl}:1:${latlon_dims}" \
      -set_var UOGRD -set center 7 \
      -set_lev "${depth} m below water surface" \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2 \
    -import_netcdf "${infile}" "vo" "0:1:${zl}:1:${latlon_dims}" \
      -set_var VOGRD -set center 7 \
      -set_lev "${depth} m below water surface" \
      -set_date "${current_cycle}" -set_ftime "${aperiod} hour ave fcst" \
      -set_scaling same same -set_grib_type c1 -grib_out tmp.gb2

    rc=$?
    # Check if the conversion was successful
    if (( rc != 0 )); then
      echo "FATAL ERROR: Failed to convert the ocean rectilinear netCDF file to grib2 format at depth ${depth}m, ABORT!"
      return "${rc}"
    fi

    cat tmp.gb2 >> "${outfile}"
    rm -f tmp.gb2
    ((zl = zl + 1))

  done

  # Notes:
  #   WATPTEMP (water potential temperature (theta)) may be a better
  #   GRIB2 parameter than WTMP (water temperature) if MOM6 outputs
  #   potential temperature. WATPTEMP is not available in NCEP
  #   (-set center 7) tables in wgrib2 v2.0.8.

  return "${rc}"

}

################################################################################
# Input arguments
component=${1:?"Need a valid component; options: ice|ocean"}
grid=${2:-"0p25"} # Default to 0.25-degree grid
current_cycle=${3:-"2013100100"} # Default to 2013100100
avg_period=${4:-"0-6"} # Default to 6-hourly average
ocean_levels=${5:-"5:15:25:35:45:55:65:75:85:95:105:115:125"} # Default to 12-levels

case "${grid}" in
  "0p25")
    latlon_dims="0:721:0:1440"
  ;;
  "0p50")
    latlon_dims="0:361:0:720"
  ;;
  "1p00")
    latlon_dims="0:181:0:360"
  ;;
  "5p00")
    latlon_dims="0:36:0:72"
  ;;
  *)
    echo "FATAL ERROR: Unsupported grid '${grid}', ABORT!"
    exit 1
  ;;
esac

input_file="${component}.${grid}.nc"
template="template.global.${grid}.gb2"

# Check if the template file exists
if [[ ! -f "${template}" ]]; then
  echo "FATAL ERROR: '${template}' does not exist, ABORT!"
  exit 127
fi

# Check if the input file exists
if [[ ! -f "${input_file}" ]]; then
  echo "FATAL ERROR: '${input_file}' does not exist, ABORT!"
  exit 127
fi

case "${component}" in
  "ice")
    rm -f "${component}.${grid}.grib2" || true
    _ice_nc2grib2 "${grid}" "${latlon_dims}" "${current_cycle}" "${avg_period}" "${input_file}" "${component}.${grid}.grib2" "${template}"
    rc=$?
    if (( rc != 0 )); then
      echo "FATAL ERROR: Failed to convert the ice rectilinear netCDF file to grib2 format"
      exit "${rc}"
    fi
  ;;
  "ocean")
    rm -f "${component}_2D.${grid}.grib2" || true
    _ocean2D_nc2grib2 "${grid}" "${latlon_dims}" "${current_cycle}" "${avg_period}" "${input_file}" "${component}_2D.${grid}.grib2" "${template}"
    rc=$?
    if (( rc != 0 )); then
      echo "FATAL ERROR: Failed to convert the ocean 2D rectilinear netCDF file to grib2 format"
      exit "${rc}"
    fi
    rm -f "${component}_3D.${grid}.grib2" || true
    _ocean3D_nc2grib2 "${grid}" "${latlon_dims}" "${ocean_levels}" "${current_cycle}" "${avg_period}" "${input_file}" "${component}_3D.${grid}.grib2" "${template}"
    rc=$?
    if (( rc != 0 )); then
      echo "FATAL ERROR: Failed to convert the ocean 3D rectilinear netCDF file to grib2 format"
      exit "${rc}"
    fi
    # Combine the 2D and 3D grib2 files into a single file
    rm -f "${component}.${grid}.grib2" || true
    cat "${component}_2D.${grid}.grib2" "${component}_3D.${grid}.grib2" > "${component}.${grid}.grib2"

  ;;
  *)
    echo "FATAL ERROR: Unknown component: '${component}'. ABORT!"
    exit 3
  ;;
esac

# Index the output grib2 file
${WGRIB2} -s "${component}.${grid}.grib2" > "${component}.${grid}.grib2.idx"
rc=$?
# Check if the indexing was successful
if (( rc != 0 )); then
  echo "FATAL ERROR: Failed to index the file '${component}.${grid}.grib2'"
  exit "${rc}"
fi

exit 0
