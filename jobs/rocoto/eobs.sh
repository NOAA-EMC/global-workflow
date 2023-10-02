#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

# TODO: clean this up
#Source appropriate modulefiles based on machine

source "${HOMEgfs}/ush/detect_machine.sh"
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
    # Source FV3GFS workflow modules
    . "${HOMEgfs}"/ush/load_fv3gfs_modules.sh
    status=$?
    [[ ${status} -ne 0 ]] && exit "${status}"
else
    # Append compiler (only on machines that have multiple compilers)
    COMPILER=${COMPILER:-"intel"}
    if [[ "${MACHINE_ID}" = "hera" ]] || [[ "${MACHINE_ID}" = "cheyenne" ]]; then
        MACHINE_ID=${MACHINE_ID}.${COMPILER}
    fi

    # Source machine specific GSI-EnKF modules
    set +x
    source "${HOMEgfs}/ush/module-setup.sh"
    module use "${HOMEgfs}/sorc/gsi_enkf.fd/modulefiles"
    module load gsi_"${MACHINE_ID}"

    if [[ "${MACHINE_ID}" = "orion" ]]; then
       module load miniconda3/4.12.0
       set +u
       conda activate regional_workflow_cmaq
       set_strict
    fi

    module list
    unset MACHINE_ID
    set_trace
fi

export job="eobs"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGDAS_ENKF_SELECT_OBS
status=$?


exit ${status}
