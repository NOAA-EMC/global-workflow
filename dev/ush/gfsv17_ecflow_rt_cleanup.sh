#!/bin/bash
set -eu
declare -x PS4='+ $(basename ${BASH_SOURCE[0]:-${FUNCNAME[0]:-"Unknown"}})[${LINENO}]'

# Add a dry-run option
if [[ "${1:-}" == "--dry-run" ]]; then
    echo "Dry run mode: No files will be deleted."
    DRY_RUN=true
else
    DRY_RUN=false
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

cd "${DATA}"
setpdy.sh
source PDY
# PDYm1 PDY PDYp1

echo "Start cleanup at $(date)"

# Exception handling - if the realtime state is delay
if [[ ! -d "${COMROOT}/enkfgdas.${PDY}/06" ]]; then
    echo "FATAL ERROR: The ${COMROOT}/enkfgdas.${PDY}/06 is not found"
    exit 9
fi

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
for dir_to_remove in $(find ./* -maxdepth 0 -type d -mmin +2880 | grep -v "${PDY}" | grep -v "${PDYm1}" | grep -v "fix" | grep -v "syndat"); do
    echo "Removing directory ${COMROOT}/${dir_to_remove}"
    if [[ "${DRY_RUN}" == false ]]; then
        rm -rf "${dir_to_remove}"
    fi
done

echo "Finished cleaning up at $(date)"
