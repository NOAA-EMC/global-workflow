#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="postsnd"

# shellcheck disable=SC2153
if [[ "${RUN}" == "gefs" ]]; then
    IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"
    export FHR3 jobid
    for fhr in "${fhr_list[@]}"; do
        FHR3=$(printf '%03d' "${fhr}")
        jobid="${job}_f${FHR3}.$$"
        echo "add J-script here for GEFS."
    done
else
    export jobid="${job}.$$"
    ################################################################
    # Execute the JJOB
    ${HOMEgfs}/jobs/J${RUN^^}_ATMOS_POSTSND
    err=$?
    exit "${err}"
fi
