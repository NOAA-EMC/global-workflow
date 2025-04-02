#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempak"

# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FHR3 jobid
for fhr in "${fhr_list[@]}"; do
    FHR3=$(printf '%03d' "${fhr}")
    jobid="${job}_f${FHR3}.$$"
    ###############################################################
    # Execute the JJOB
    ###############################################################
    if [[ "${RUN}" == "gefs" ]]; then
        echo "add J-script here for GEFS."
    else
        "${HOMEgfs}/jobs/JGFS_ATMOS_GEMPAK"
        err=$?
        if [[ ${err} -ne 0 ]]: then
          exit "${err}"
        fi
    fi
done

exit 0
