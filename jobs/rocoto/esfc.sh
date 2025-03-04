#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
# For DO_GSISOILDA=YES need to switch to ufsda modules 
# until g-w issue 3390 is resolved.
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#. ${HOMEgfs}/ush/load_ufsda_modules.sh
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="esfc"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGDAS_ENKF_SFC
status=$?


exit "${status}"
