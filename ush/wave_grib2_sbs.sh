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


# Script inputs
grdID=$1
GRIDNR=$2
MODNR=$3
valid_time=$4
fhr=$5
grid_region=$6
grid_res=$7
grib_flags=$8

cd "${DATA}" || exit 99

grib_DATA="${DATA}/grib_${grdID}"
rm -rf "${grib_DATA}"
mkdir -p "${grib_DATA}"
cd "${grib_DATA}" || exit 99

# SBS one time slice per file
FH3=$(printf %03i "${fhr}")

# create the COM directory var
com_varname="COMOUT_WAVE_GRID_${grid_region}_${grid_res}"
com_dir="${!com_varname}"
mkdir -p "${com_dir}"

# Verify if grib2 file exists from interrupted run
outfile="${RUN}.t${cyc}z.${grid_region}.${grid_res}.f${FH3}.grib2"

# Check if outfile exists in COM
if [[ -s "${com_dir}/${outfile}" ]] && [[ -s "${com_dir}/${outfile}.idx" ]]; then
  echo "File ${com_dir}/${outfile}[.idx] found, skipping generation process"
  exit 0
fi

# Copy template files to grib_DATA (required for ww3_grib.x)
cpreq "${PARMgfs}/wave/ww3_grib2.${grdID}.inp.tmpl" "./ww3_grib2.${grdID}.inp.tmpl"

# Link mod_def files from DATA into grib_DATA
${NLN} "${DATA}/mod_def.${grdID}" "./mod_def.ww3"

# Link interpolated output from DATA (WW3 or ww3_gint.x generated this file)
${NLN} "${DATA}/out_grd.${grdID}" "./out_grd.ww3"

# Create the input file for the ww3_grib2 code
ngrib=1      # only one time slice
dtgrib=3600  # only one time slice
tstart="${valid_time:0:8} ${valid_time:8:2}0000"

sed -e "s/TIME/${tstart}/g" \
    -e "s/DT/${dtgrib}/g" \
    -e "s/NT/${ngrib}/g" \
    -e "s/GRIDNR/${GRIDNR}/g" \
    -e "s/MODNR/${MODNR}/g" \
    -e "s/FLAGS/${grib_flags}/g" \
    "ww3_grib2.${grdID}.inp.tmpl" > ww3_grib.inp
cat ww3_grib.inp

# Run the ww3_grib generation code
export pgm="${NET,,}_ww3_grib.x"
source prep_step
"${EXECgfs}/${pgm}" > "grib2_${grid_region}_${FH3}.out" 2>&1
export err=$?
if [[ ${err} -ne 0 ]]; then
   echo "FATAL ERROR: ${pgm} returned non-zero status: ${err}; exiting!"
   exit "${err}"
fi
cat "grib2_${grid_region}_${FH3}.out"

if [[ ! -s gribfile ]]; then
  echo "FATAL ERROR: '${pgm}' failed!"
  exit 2
fi

if [[ ${fhr} -gt 0 ]]; then
  ${WGRIB2} gribfile -set_date "${PDY}${cyc}" -set_ftime "${fhr} hour fcst" -grib "${outfile}"
  err=$?
else
  ${WGRIB2} gribfile -set_date "${PDY}${cyc}" -set_ftime "${fhr} hour fcst" \
    -set table_1.4 1 -set table_1.2 1 -grib "${outfile}"
  err=$?
fi

if [[ ${err} -ne 0 ]]; then
  echo "FATAL ERROR: Error creating '${outfile}' with '${WGRIB2}'"
  exit 3
fi

# Create index
${WGRIB2} -s "${outfile}" > "${outfile}.idx"

# Move grib files to COM directory
if [[ -s "${outfile}" && -s "${outfile}.idx" ]]; then
  cpfs "${outfile}"     "${com_dir}/${outfile}"
  cpfs "${outfile}.idx" "${com_dir}/${outfile}.idx"
  echo "INFO: Copied ${outfile} and ${outfile}.idx from ${grib_DATA} to COM"
else
  echo "FATAL ERROR: ${outfile} and ${outfile}.idx not found in ${grib_DATA} to copy to COM"
  exit 4
fi

# Create grib2 subgrid if this is the source grid
if [[ "${grdID}" == "${WAV_SUBGRBSRC}" ]]; then
  for subgrb in ${WAV_SUBGRB}; do
    subgrbref=$(echo ${!subgrb} | cut -d " " -f 1-20)
    subgrbnam=$(echo ${!subgrb} | cut -d " " -f 21)
    subgrbres=$(echo ${!subgrb} | cut -d " " -f 22)
    subfnam="${RUN}.t${cyc}z.${subgrbnam}.${subgrbres}.f${FH3}.grib2"

    ${COPYGB2} -g "${subgrbref}" -i0 -x "${outfile}" "${subfnam}"
    ${WGRIB2} -s "${subfnam}" > "${subfnam}.idx"

    if [[ -s "${subfnam}" && -s "${subfnam}.idx" ]]; then
      cpfs "${subfnam}"     "${com_dir}/${subfnam}"
      cpfs "${subfnam}.idx" "${com_dir}/${subfnam}.idx"
      echo "INFO: Copied ${subfnam} and ${subfnam}.idx from ${GRIBDATA} to COM"
    else
      echo "FATAL ERROR: ${subfnam} and ${subfnam}.idx not found in ${grib_DATA} to copy to COM"
      exit 5
    fi
  done
fi

if [[ "${SENDDBN}" == 'YES' && "${outfile}" != *global.0p50* ]]; then
  echo "INFO: Alerting GRIB file as ${outfile}"
  echo "INFO: Alerting GRIB index file as ${outfile}.idx"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_WAVE_GB2" "${job}" "${com_dir}/${outfile}"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_WAVE_GB2_WIDX" "${job}" "${com_dir}/${outfile}.idx"
else
  echo "INFO: ${outfile} is global.0p50 or SENDDBN is NO, no alert sent"
fi
