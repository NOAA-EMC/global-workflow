#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_grid_interp_sbs.sh
# Script description:  Interpolate from native grids to target grid
#
# Author:   J-Henrique Alves    Org: NCEP/EMC      Date: 2019-11-02
# Abstract: Creates grib2 files from WW3 binary output (FIXME: No, this does not create grib2 files)
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
################################################################################

# 0.a Basic modes of operation

grdID=$1
valid_time=$2
dt=$3
nst=$4

cd "${DATA}" || exit 99
interp_DATA="${DATA}/grid_interp_${grdID}"
rm -rf "${interp_DATA}"
mkdir -p "${interp_DATA}"
cd "${interp_DATA}" || exit 99

# Copy template files to interp_DATA (required for interpolation)
cpreq "${PARMgfs}/wave/ww3_gint.inp.tmpl" "ww3_gint.inp.tmpl"

# Link input files (WW3 output) from DATA into interp_DATA
${NLN} "${DATA}/out_grd.${waveGRD}" "./out_grd.${waveGRD}"

# Link mod_def files from DATA into interp_DATA
for ID in ${waveGRD} ${grdID}; do
  ${NLN} "${DATA}/mod_def.${ID}" "./mod_def.${ID}"
done

# Check if there is an interpolation weights file available, and copy it if so
if [[ -f "${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${waveGRD}.${grdID}" ]]; then
  echo "INFO: Interpolation weights found at: '${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${waveGRD}.${grdID}'"
  cpreq "${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${waveGRD}.${grdID}" "./WHTGRIDINT.bin"
  weights_found=1
else
  echo "WARNING: No weights file found at: '${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${waveGRD}.${grdID}'"
  echo "INFO: Interpolation will create a new weights file"
  weights_found=0
fi

# Create the input file for the interpolation code
ymdhms="${valid_time:0:8} ${valid_time:8:2}0000"
sed -e "s/TIME/${ymdhms}/g" \
    -e "s/DT/${dt}/g" \
    -e "s/NSTEPS/${nst}/g" \
    -e "s/GRIDIN/${waveGRD}/g" \
    -e "s/GRIDOUT/${grdID}/g" \
    "ww3_gint.inp.tmpl" > ww3_gint.inp
cat ww3_gint.inp

# Run the interpolation code
export pgm="${NET,,}_ww3_gint.x"
source prep_step
echo "INFO: Executing '${pgm}'"
"${EXECgfs}/${pgm}" > grid_interp.${grdID}.out 2>&1
cat "grid_interp.${grdID}.out"
if [[ ${err} -ne 0 ]]; then
  echo "FATAL ERROR: '${pgm}' failed!"
  exit 3
fi

if [[ ${weights_found} -eq 0 ]]; then
  echo "INFO: Interpolation created a new weights file at: '${interp_DATA}/WHTGRIDINT.bin'"
fi

# Link output file (interpolated output) within DATA (this program generates this file)
if [[ -f "./out_grd.${grdID}" ]]; then
  if [[ -f "${DATA}/out_grd.${grdID}" ]]; then
    echo "FATAL ERROR: '${DATA}/out_grd.${grdID}' already exists, ABORT!"
    exit 4
  else
    ${NLN} "${interp_DATA}/out_grd.${grdID}" "${DATA}/out_grd.${grdID}"
  fi
else
  echo "FATAL ERROR: '${pgm}' failed to generate output file at: '${interp_DATA}/out_grd.${grdID}'"
  exit 4
fi
