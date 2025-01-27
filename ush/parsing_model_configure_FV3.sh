#! /usr/bin/env bash

# parsing model_configure for UFSWM FV3

# shellcheck disable=SC2034
FV3_model_configure(){

local restile=${CASE:1}

# Prepare local variables for use in model_configure.IN from UFSWM
# The ones already defined are left commented as a reminder

local model_start_date
if [[ "${DOIAU}" == "YES" ]]; then
  model_start_date="${previous_cycle}"
else
  model_start_date="${current_cycle}"
fi

local SYEAR=${model_start_date:0:4}
local SMONTH=${model_start_date:4:2}
local SDAY=${model_start_date:6:2}
local SHOUR=${model_start_date:8:2}
# FHMAX
local FHROT=${IAU_FHROT:-0}
local DT_ATMOS=${DELTIM}
local RESTART_INTERVAL="${FV3_RESTART_FH[*]}"
local RESTART_FH="${CMEPS_RESTART_FH:-" "}"
# QUILTING
local QUILTING_RESTART="${QUILTING_RESTART:-${QUILTING}}"
local WRITE_GROUP=${WRITE_GROUP:-1}
local WRTTASK_PER_GROUP=${WRTTASK_PER_GROUP:-24}
local ITASKS=1
local OUTPUT_HISTORY=${OUTPUT_HISTORY:-".true."}
if [[ "${DO_JEDIATMVAR:-}" == "YES" ]]; then
  local HISTORY_FILE_ON_NATIVE_GRID=".true."
else
  local HISTORY_FILE_ON_NATIVE_GRID=".false."
fi
local WRITE_DOPOST=${WRITE_DOPOST:-".false."}
local WRITE_NSFLIP=${WRITE_NSFLIP:-".false."}
local NUM_FILES=${NUM_FILES:-2}
local FILENAME_BASE="'atm' 'sfc'"
# OUTPUT_GRID
local OUTPUT_FILE="'${OUTPUT_FILETYPE_ATM}' '${OUTPUT_FILETYPE_SFC}'"
local ZSTANDARD_LEVEL=${zstandard_level:-0}
local IDEFLATE=${ideflate:-0}  # netCDF zlib lossless compression (0-9); 0: no compression
local QUANTIZE_NSD=${quantize_nsd:-0}  # netCDF compression
local ICHUNK2D=$((4*restile))
local JCHUNK2D=$((2*restile))
local ICHUNK3D=$((4*restile))
local JCHUNK3D=$((2*restile))
local KCHUNK3D=1
local IMO=${LONB_IMO}
local JMO=${LATB_JMO}
local OUTPUT_FH=${FV3_OUTPUT_FH_NML}
local IAU_OFFSET=${IAU_OFFSET:-0}

# Ensure the template exists
if [[ "${DO_NEST:-NO}" == "YES" ]] ; then
  local NEST_IMO=${npx_nest}
  local NEST_JMO=${npy_nest}
  template="${PARMgfs}/ufs/input_global_nest.nml.IN"
else
  template="${PARMgfs}/ufs/model_configure.IN"
fi
if [[ ! -f ${template} ]]; then
  echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
  exit 1
fi
rm -f "${DATA}/model_configure"
atparse < "${template}" >> "${DATA}/model_configure"
echo "Rendered model_configure"
cat "${DATA}/model_configure"

}
