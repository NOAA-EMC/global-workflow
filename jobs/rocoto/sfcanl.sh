#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
# CSD for GSI_SOILANAL, need ufsda modules (temporary, until merged)
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#. ${HOMEgfs}/ush/load_ufsda_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="sfcanl"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGLOBAL_ATMOS_SFCANL
status=$?


exit ${status}
