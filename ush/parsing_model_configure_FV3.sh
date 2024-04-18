#! /usr/bin/env bash

# parsing model_configure for UFSWM FV3

# shellcheck disable=SC2034
FV3_model_configure(){

local restile=${CASE:1}

# Prepare local variables for use in model_configure.IN from UFSWM
# The ones already defined are left commented as a reminder
local SYEAR=${tPDY:0:4}
local SMONTH=${tPDY:4:2}
local SDAY=${tPDY:6:2}
local SHOUR=${tcyc}
# FHMAX
local FHROT=${IAU_FHROT:-0}
local DT_ATMOS=${DELTIM}
local RESTART_INTERVAL="${restart_interval} -1"
# QUILTING
local QUILTING_RESTART=".true."
local WRITE_GROUP=${WRITE_GROUP:-1}
local WRTTASK_PER_GROUP=${WRTTASK_PER_GROUP:-24}
local ITASKS=1
local OUTPUT_HISTORY=${OUTPUT_HISTORY:-".true."}
local HISTORY_FILE_ON_NATIVE_GRID=".false."
local WRITE_DOPOST=${WRITE_DOPOST:-".false."}
local WRITE_NSFLIP=${WRITE_NSFLIP:-".false."}
local NUM_FILES=${NUM_FILES:-2}
local FILENAME_BASE="'atm' 'sfc'"
# OUTPUT_GRID
local OUTPUT_FILE="'${OUTPUT_FILETYPE_ATM}' '${OUTPUT_FILETYPE_SFC}'"
local ZSTANDARD_LEVEL=0
local IDEFLATE=1  # netCDF zlib lossless compression (0-9); 0: no compression
local QUANTIZE_NSD=${QUANTIZE_NSD:-0}  # netCDF compression
local ICHUNK2D=$((4*restile))
local JCHUNK2D=$((2*restile))
local ICHUNK3D=$((4*restile))
local JCHUNK3D=$((2*restile))
local KCHUNK3D=1
local IMO=${LONB_IMO}
local JMO=${LATB_JMO}
local OUTPUT_FH=${FV3_OUTPUT_FH}
local IAU_OFFSET=${IAU_OFFSET:-0}

# Ensure the template exists
template="${PARMgfs}/ufs/model_configure.IN"
if [[ ! -f ${template} ]]; then
  echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
  exit 1
fi
source "${USHgfs}/atparse.bash"
rm -f "${DATA}/model_configure"
atparse < "${template}" >> "${DATA}/model_configure"
echo "Rendered model_configure"
cat "${DATA}/model_configure"

}
