#!/bin/bash
set -eu
declare -x PS4='+ $(basename ${BASH_SOURCE[0]:-${FUNCNAME[0]:-"Unknown"}})[${LINENO}]'

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

# Clean DATA
# DATA retain current PDY   ~   60TB
cd "${DATAROOT}"
excluded_pdy=${PDY}
excluded_cdate_m1=${PDYm1}18
for dir_to_remove in $(find * -maxdepth 0 -type d | grep -v "${excluded_pdy}" | grep -v "${excluded_cdate_m1}" | grep -v "DBNLOG" | grep -v "ecflow"); do
    dir_to_remove="${dir_to_remove%/}"
    echo "Removing directory ${DATAROOT}/${dir_to_remove}"
    rm -rf "${DATAROOT}/${dir_to_remove}"
done


# Clean COM
# COM retain 3 full days - 195TB (65T /day on production frequency)
cd "${COMROOT}"
for dir_to_remove in $(find * -maxdepth 0 -type d | grep -v "${PDY}" | grep -v "${PDYm1}" | grep -v "${PDYm2}" | grep -v "${PDYm3}" | grep -v "fix"); do
    dir_to_remove="${dir_to_remove%/}"
    echo "Removing directory ${COMROOT}/${dir_to_remove}"
    rm -rf "${COMROOT}/${dir_to_remove}"
done

echo "Finished cleaning up at $(date)"
