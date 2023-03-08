#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

set +x
module use "${HOMEgfs}/sorc/ufs_model.fd/tests"
module load modules.ufs_model.lua
# Workflow needs utilities from prod_util (setPDY.sh, ndate, etc.)
module load prod_util
module list
set_trace

export job="efcs"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGDAS_ENKF_FCST
status=$?

exit ${status}
