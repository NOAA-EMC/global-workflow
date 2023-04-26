#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

MOM6REGRID="${MOM6REGRID:-${HOMEgfs}}"
export EXEC_DIR="${MOM6REGRID}/exec"
export USH_DIR="${MOM6REGRID}/ush"
export COMOUTocean="${COM_OCEAN_HISTORY}"
export COMOUTice="${COM_ICE_HISTORY}"
export IDATE="${IDATE}"
export VDATE="${VDATE}"
export ENSMEM="${ENSMEM}"
export FHR="${fhr}"
export DATA="${DATA}"
export FIXreg2grb2="${FIXreg2grb2}"

###### DO NOT MODIFY BELOW UNLESS YOU KNOW WHAT YOU ARE DOING #######
#Need NCL module to be loaded:
echo "${NCARG_ROOT}"
export NCL="${NCARG_ROOT}/bin/ncl"

ls -alrt

${NCL} "${USH_DIR}/icepost.ncl"
${NCL} "${USH_DIR}/ocnpost.ncl"
#####################################################################

