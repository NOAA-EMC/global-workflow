#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## NCEP atmanlpost driver script
###############################################################

# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="atmanlpost"
export jobid="${job}.$$"

${HOMEgfs}/jobs/JGLOBAL_ATMOSANL_POST
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

exit 0
