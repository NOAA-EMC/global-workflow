#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_grid_interp_sbs.sh
# Script description:  Interpolate from native grids to target grid
#
# Author:   J-Henrique Alves    Org: NCEP/EMC      Date: 2019-11-02
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

# 0.a Basic modes of operation

cd "${DATA}" || exit 2

# shellcheck disable=SC2034
{
  grdID=$1
  verif_date=$2
  dt=$3
  nsteps=$4
  fhr=$5
}

echo "INFO: Making GRID Interpolation Files for ${grdID}"
interp_data="grid_interp_${grdID}"
rm -rf "${interp_data}"
mkdir "${interp_data}"
err=$?
if [[ "${err}" != '0' ]]; then
  echo 'FATAL ERROR: Could not create temp directory'
  exit 1
fi

cd "${interp_data}" || exit 2

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

echo 'INFO: Make interpolated grid files'

# 0.c Links to files

if [[ ! -f "${DATA}/${grdID}_interp.inp.tmpl" ]]; then
  cp "${PARMgfs}/wave/${grdID}_interp.inp.tmpl" "${DATA}/${grdID}_interp.inp.tmpl"
fi
${NLN} "../${grdID}_interp.inp.tmpl" "${grdID}_interp.inp.tmpl"

# Link input file within DATA
${NLN} "../out_grd.${waveGRD}" "out_grd.${waveGRD}"

# Link output file within DATA
${NLN} "../out_grd.${grdID}" "out_grd.${grdID}"

for id in ${waveGRD} ${grdID}; do
  ${NLN} "../mod_def.${id}" "mod_def.${id}"
done


# --------------------------------------------------------------------------- #
# 1.  Generate GRID file with all data
# 1.a Generate Input file

# shellcheck disable=SC2034
time="${verif_date:0:8} ${verif_date:8:2}0000"
atparse < "${grdID}_interp.inp.tmpl" > ww3_gint.inp

# Check if there is an interpolation weights file available

weights_exist='no'
if [[ ! -f "${DATA}/ww3_gint.WHTGRIDINT.bin.${grdID}" ]]; then
  if [[ -f "${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID}" ]]; then
    echo "INFO: Copying ${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID}"
    cp "${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID}" "${DATA}/ww3_gint.WHTGRIDINT.bin.${grdID}"
    weights_exist='yes'
  else
    echo "INFO: Not found: ${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID}"
  fi
fi
# Check and link weights file
if [[ -f "${DATA}/ww3_gint.WHTGRIDINT.bin.${grdID}" ]]; then
  ${NLN} "../ww3_gint.WHTGRIDINT.bin.${grdID}" "./WHTGRIDINT.bin"
fi

# 1.b Run interpolation code

export pgm="${NET,,}_ww3_gint.x"
source prep_step

echo "INFO: Executing ${pgm}"

"${EXECgfs}/${pgm}" 1> "gint.${grdID}.out" 2>&1
export err=$?; err_chk

# Write interpolation file to main TEMP dir area if not there yet
if [[ "${weights_exist}" == 'no' ]]; then
  cp -f "./WHTGRIDINT.bin" "../ww3_gint.WHTGRIDINT.bin.${grdID}"
fi

if [[ "${err}" != '0' ]]; then
  echo "FATAL ERROR: Error in ${pgm} interpolation"
  exit 3
fi

# 1.b Save in /com

# WCK - I don't think these actually need to be sent to COM
#  Check with Jessica

# outfile="${RUN}.wave.${grdID}.f${fhr3}.bin"
# echo "INFO: Saving GRID file as ${COMOUT_WAVE_PREP}/${outfile}"
# cpfs "out_grd.${grdID}" "${COMOUT_WAVE_PREP}/${outfile}"

# End of ww3_grid_interp.sh -------------------------------------------- #
