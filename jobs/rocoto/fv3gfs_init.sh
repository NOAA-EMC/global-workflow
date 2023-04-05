#! /usr/bin/env bash

# HRW source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

# TODO: clean this up
# HRW source "${HOMEgfs}/ush/detect_machine.sh"
# HRW set +x
# HRW source "${HOMEgfs}/ush/module-setup.sh"
# HRW module use "${HOMEgfs}/sorc/ufs_model.fd/tests"
# HRW module load modules.ufs_model.lua
# HRW module load prod_util
# HRW if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
# HRW  module load cray-pals
# HRW fi
# HRW module list
# HRW unset MACHINE_ID
#HRW set_trace

export job="fcst"
export jobid="${job}.refactor" # HRW

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGLOBAL_FORECAST_GFS_INITIALIZE
status=$?


exit ${status}
