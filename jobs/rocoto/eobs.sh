#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="eobs"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGDAS_ENKF_SELECT_OBS
status=$?


exit ${status}
