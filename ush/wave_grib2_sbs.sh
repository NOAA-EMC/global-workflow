#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_grib2_sbs.sh
# Script description:  Create grib2 files for the wave component
#
# Author:   Hendrik Tolman      Org: NCEP/EMC      Date: 2007-07-11
# Abstract: Creates grib2 files from WW3 binary output
#
# Script history log:
# 2019-11-02  J-Henrique Alves Ported to global-workflow.
# 2020-06-10  J-Henrique Alves Ported to R&D machine Hera
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (BASH) shell
#
# Requirements:
# - wgrib2 with IPOLATES library
#
################################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"
source "${USHgfs}/atparse.bash"

# 0.a Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

# shellcheck disable=SC2034
{
  grdID=$1
  gridnr=$2
  modnr=$3
  valid_time=$4
  fhr=$5
  grid_region=$6
  grid_res=$7
  grib_flags=$8
}

# 0.b Basic modes of operation

cd "${DATA}" || exit 2

grib_data="grib_${grdID}"
rm -rf "${grib_data}"
mkdir "${grib_data}"
err=$?
if [[ ${err} != 0 ]]; then
  echo "FATAL ERROR: Could not create temp directory ${grib_data}"
  exit 1
fi

cd "${grib_data}" || exit 2

# SBS one time slice per file
fhr3=$(printf %03i "${fhr}")

#create the COM directory var
com_varname="COMOUT_WAVE_GRID_${grid_region}_${grid_res}"
comout="${!com_varname}"

# Verify if grib2 file exists from interrupted run
outfile="${RUN}.wave.${cycle}.${grid_res}.f${fhr3}.${grid_region}.grib2"

# Only create file if not present in COM
if [[ ! -s "${comout}/${outfile}.idx" ]]; then

  # 0.c Starting time for output

  time="${valid_time:0:8} ${valid_time:8:2}0000"

  cat << EOF
    Starting time    : ${time}
    GRIB field flags : ${grib_flags}
EOF

  # 0.e Links to working directory

  ${NLN} "../mod_def.${grdID}" "mod_def.ww3"
  ${NLN} "../out_grd.${grdID}" "out_grd.ww3"

  # --------------------------------------------------------------------------- #
  # 1.  Generate GRIB file with all data
  # 1.a Generate input file for ww3_grib2
  #     Template copied in mother script ...

  echo "INFO: Generate input file for ww3_grib2"

  # shellcheck disable=SC2034
  {
    nt=1 # only one time slice
    dt=3600 # only one time slice
  }
  atparse < "${DATA}/ww3_grib2.${grdID}.inp.tmpl" > ww3_grib.inp

  echo "ww3_grib.inp"
  cat ww3_grib.inp

  # 1.b Run GRIB packing program

  export pgm="${NET,,}_ww3_grib.x"
  source prep_step

  echo "INFO: Executing ${EXECgfs}/${pgm}"

  "${EXECgfs}/${pgm}"
  export err=$?; err_chk
  if [[ ! -s gribfile ]]; then
    echo "FATAL ERROR: Error in ${pgm} encoding"
    exit 3
  fi

  if (( fhr > 0 )); then
    ${WGRIB2} gribfile -set_date "${PDY}${cyc}" -set_ftime "${fhr} hour fcst" -grib "${outfile}"
    err=$?
  else
    ${WGRIB2} gribfile -set_date "${PDY}${cyc}" -set_ftime "${fhr} hour fcst" \
      -set 'table_1.4' '1' -set 'table_1.2' '1' -grib "${outfile}"
    err=$?
  fi

  if [[ ${err} != 0 ]]; then
    echo "FATAL ERROR: Error setting grib2 parameters with wgrib2"
    exit 3
  fi

  # Create index
  ${WGRIB2} -s "${outfile}" > "${outfile}.idx"

  # Create grib2 subgrid is this is the source grid
  if [[ "${grdID}" == "${WAV_SUBGRBSRC}" ]]; then
    subgrid_filenames=()
    for subgrib_varname in ${WAV_SUBGRB}; do
      subgrib=${!subgrib_varname}
      subgrib_ref=$(cut -d " " -f 1-20 <<< "${subgrib}")
      subgrib_name=$(cut -d " " -f 21 <<< "${subgrib}")
      subgrib_res=$(cut -d " " -f 22 <<< "${subgrib}")
      subgrib_filename="${RUN}.wave.${cycle}.${subgrib_name}.${subgrib_res}.f${fhr3}.grib2"
      ${COPYGB2} -g "${subgrib_ref}" -i0 -x "${outfile}" "${subgrib_filename}"
      ${WGRIB2} -s "${subgrib_filename}" > "${subgrib_filename}.idx"
      # Save filenames to copy to COM later
      subgrib_filenames+=("${subgrib_filename}")
   done
  fi

  # 1.e Save in /com

  # Check if the COM directory exists, create it if necessary
  if [[ ! -d "${comout}" ]]; then
      mkdir -p -m "${comout}"
      echo "INFO: Directory ${comout} created."
  fi

  # Copy main grib files
  cpfs "${outfile}" "${comout}/${outfile}"
  cpfs "${outfile}.idx" "${comout}/${outfile}.idx"

  # Copy subgrid files
  for subgrid_filename in "${subgrid_filenames[@]}"; do
    cpfs "${subgrid_filename}" "${comout}/${subgrid_filename}"
    cpfs "${subgrid_filename}.idx" "${comout}/${subgrid_filename}.idx"
  done

  if [[ "${SENDDBN}" = 'YES' ]] && [[ ${outfile} != *global.0p50* ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_WAVE_GB2" "${job}" "${comout}/${outfile}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_WAVE_GB2_WIDX" "${job}" "${comout}/${outfile}.idx"
  else
    echo "INFO: ${outfile} is global.0p50 or SENDDBN is NO, no alert sent"
  fi

else
  echo "INFO: File ${comout}/${outfile} already exists, skipping generation"
fi

# Verify grib2 file created
if [[ ! -s ${comout}/${outfile} ]]; then
  echo "FATAL ERROR: ${comout}/${outfile} not generated"
  err=5; export err; ${errchk}
  exit "${err}"
fi
