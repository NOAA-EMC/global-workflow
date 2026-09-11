#!/bin/bash
set -eu
declare -x PS4='+ $(basename ${BASH_SOURCE[0]:-${FUNCNAME[0]:-"Unknown"}})[${LINENO}]'

# Add a dry-run option
if [[ "${1:-}" == "--dry-run" ]]; then
    echo "Dry run mode: No files will be deleted."
    DRY_RUN=true
elif [[ "${1:-}" == "--go" ]]; then
    DRY_RUN=false
elif [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    echo "Usage: $0 [--dry-run | --go]"
    echo "  --dry-run : Show what would be deleted without actually deleting anything."
    echo "  --go      : Perform the cleanup and delete files."
    exit 0
else
    echo "Invalid option. Use --help for usage information."
    exit 1
fi

set -x

COMROOT=/lfs/h2/emc/gfstemp/emc.global/ecflow/comroot/ops/para/com/gfs/v17.0
DATAROOT=/lfs/h2/emc/gfstemp/emc.global/ecflow/rundirs
aux_EXPDIR=/lfs/h2/emc/gfstemp/emc.global/aux

mkdir -p "${DATAROOT}"
PDY=$(date +%Y%m%d)
export PDY
export cycle=t00z

# Exception handling - if the realtime state has fallen behind, then subtract one from the PDY and try again.
max_tries=5
found=0
attempts=0
while [[ ${found} -eq 0 && ${attempts} -lt ${max_tries} ]]; do
    attempts=$((attempts + 1))
    hours=$((attempts * 24))
    if [[ ! -d "${COMROOT}/enkfgdas.${PDY}/06" ]]; then
        echo "WARNING: The ${COMROOT}/enkfgdas.${PDY}/06 was not found; subtracting 1 from PDY and trying again"
        PDY=$(date +%Y%m%d --date="${hours} hours ago")
    else
        found=1
    fi
done

if [[ ${found} -ne 1 ]]; then
    echo "FATAL ERROR: Could not find any available COM data in the past 5 days. Aborting."
    exit 9
fi

PDY=$(date +%Y%m%d)
PDYm1=$(date +%Y%m%d --date="24 hours ago")

echo "Start cleanup at $(date)"

# Clean DATA directories older than 24 hours
cd "${DATAROOT}"
for dir_to_remove in $(find ./* -maxdepth 0 -type d -mmin +1440 | grep -v "DBNLOG" | grep -v "ecflow"); do
    echo "Removing directory ${DATAROOT}/${dir_to_remove}"
    if [[ "${DRY_RUN}" == false ]]; then
        rm -rf "${dir_to_remove}"
    fi
done

# Clean COM
# COM retain 2 full days
cd "${COMROOT}"
for dir_to_remove in $(find ./* -maxdepth 0 -type d -mmin +1440 | grep -v "${PDY}" | grep -v "${PDYm1}" | grep -v "fix" | grep -v "syndat" | grep -v "sdm_rtdm" | grep -v vrfyarch); do
    # Check if the auxiliary workflow is far enough along to delete the COM directory
    # The fit2obs job needs the gdas directory from the previous day. Use the aux logs to determine if it's safe to delete.
    if [[ "${dir_to_remove}" == "./gdas.20"* ]]; then
        gdas_date=${dir_to_remove#./gdas.}
        gdas_datem1=$(date +%Y%m%d --date="${gdas_date} - 1 day")
        aux_log="${aux_EXPDIR}/logs/${gdas_datem1}18.log"
        if [[ ! -f "${aux_log}" ]]; then
            echo "WARNING: The auxiliary log file ${aux_log} does not exist."
            echo "         Skipping deletion of ${COMROOT}/${dir_to_remove}."
            # Raise an error if this is more than 3 days prior to PDY
            if [[ "${gdas_date}" -lt $(date +%Y%m%d --date="${PDY} - 3 days") ]]; then
                echo "ERROR: The auxiliary log file ${aux_log} is missing and the date is more than 3 days old."
                exit 1
            fi
            continue
        fi
        if ! grep -q "This cycle is complete: Success" "${aux_log}"; then
            echo "WARNING: The fit2obs job for ${gdas_datem1} has not completed successfully. Skipping deletion of ${COMROOT}/${dir_to_remove}."
            # Raise an error if this is more than 3 days prior to PDY.
            if [[ "${gdas_date}" -lt $(date +%Y%m%d --date="${PDY} - 3 days") ]]; then
                echo "ERROR: The fit2obs job has not run successfully and ${dir_to_remove} is more than 3 days old."
                exit 1
            fi
            continue
        fi
    fi

    PDYm1=$(date +%Y%m%d --date="24 hours ago")
    echo "Removing directory ${COMROOT}/${dir_to_remove}"
    if [[ "${DRY_RUN}" == false ]]; then
        rm -rf "${dir_to_remove}"
    fi
done

# TODO: Add cleanup task for the vrfyarch directory.

echo "Finished cleaning up at $(date)"
