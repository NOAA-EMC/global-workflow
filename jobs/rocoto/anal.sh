#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

# TODO: clean this up
source "${HOMEgfs}/ush/detect_machine.sh"

# Append compiler (only on machines that have multiple compilers)
COMPILER=${COMPILER:-"intel"}
if [[ "${MACHINE_ID}" = "hera" ]] || [[ "${MACHINE_ID}" = "cheyenne" ]]; then
    MACHINE_ID=${MACHINE_ID}.${COMPILER}
fi

set +x
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/sorc/gsi_enkf.fd/modulefiles"
module load gsi_"${MACHINE_ID}"
module list
unset MACHINE_ID
set_trace

export job="anal"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGLOBAL_ATMOS_ANALYSIS
status=$?


exit ${status}
