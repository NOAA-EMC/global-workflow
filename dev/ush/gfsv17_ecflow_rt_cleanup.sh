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

module reset
module load prod_envir prod_util

module list

set -x

DATA=/lfs/h2/emc/ptmp/${USER}/gfs_v17_cleanup.$$
COMROOT=/lfs/h2/emc/gfstemp/emc.global/ecflow/comroot/ops/para/com/gfs/v17.0
DATAROOT=/lfs/h2/emc/gfstemp/emc.global/ecflow/rundirs

mkdir -p "${DATA}" "${DATAROOT}"
PDY=$("${NDATE}" | cut -c1-8)
export PDY
export cycle=t00z

# Exception handling - if the realtime state has fallen behind, then subtract one from the PDY and try again.
max_tries=5
found=0
attempts=0
while [[ ${found} -eq 0 && ${attempts} -lt ${max_tries} ]]; do
    attempts=$((attempts + 1))
    if [[ ! -d "${COMROOT}/enkfgdas.${PDY}/06" ]]; then
        echo "WARNING: The ${COMROOT}/enkfgdas.${PDY}/06 was not found; subtracting 1 from PDY and trying again"
        PDY=$("${NDATE}" -24 | cut -c1-8)
        export PDY
    else
        found=1
    fi
done

if [[ ${found} -ne 1 ]]; then
    echo "FATAL ERROR: Could not find any available COM data in the past 5 days. Aborting."
    exit 9
fi

cd "${DATA}"
setpdy.sh
source PDY
# PDYm1 PDY PDYp1

echo "Start cleanup at $(date)"

# Clean DATA directories older than 48 hours
cd "${DATAROOT}"
for dir_to_remove in $(find ./* -maxdepth 0 -type d -mmin +2880 | grep -v "DBNLOG" | grep -v "ecflow"); do
    echo "Removing directory ${DATAROOT}/${dir_to_remove}"
    if [[ "${DRY_RUN}" == false ]]; then
        rm -rf "${dir_to_remove}"
    fi
done

# Clean COM
# COM retain 2 full days
cd "${COMROOT}"
for dir_to_remove in $(find ./* -maxdepth 0 -type d -mmin +1440 | grep -v "${PDY}" | grep -v "${PDYm1}" | grep -v "fix" | grep -v "syndat" | grep -v "sdm_rtdm" | grep -v vrfyarch ); do
    echo "Removing directory ${COMROOT}/${dir_to_remove}"
    if [[ "${DRY_RUN}" == false ]]; then
        rm -rf "${dir_to_remove}"
    fi
done

# TODO: Add cleanup task for the vrfyarch directory.

echo "Finished cleaning up at $(date)"
