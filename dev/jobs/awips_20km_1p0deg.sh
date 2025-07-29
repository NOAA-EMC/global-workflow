#! /usr/bin/env bash

set -x

###############################################################
## Abstract:
## Inline awips driver script
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## RUN    : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="awips_20km_1p0deg"
export jobid="${job}.$$"

###############################################################

echo
echo "=============== BEGIN AWIPS ==============="

# shellcheck disable=SC2153
fhrlst=$(echo "${FHRLST}" | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

for fhr3 in ${fhrlst}; do
    fhr=$(( 10#${fhr3} ))
    # Process every 3 hrs from hour 0 up to hour 84
    if [[ ${fhr} -ge 0 ]] && [[ ${fhr} -le 84 ]] ; then
      if (( fhr % 3 == 0 )) ; then
        export fcsthr="${fhr3}"
        export DATA="${DATAROOT}/${jobid}.${fcsthr}"
        "${HOMEgfs}/jobs/JGFS_ATMOS_AWIPS_20KM_1P0DEG"
      fi
    # Process every 6 hrs from hour 90 up to hour 240
    elif [[ ${fhr} -ge 90 ]] && [[ ${fhr} -le 240 ]] ; then
      if (( fhr % 6 == 0 )) ; then
        export fcsthr="${fhr3}"
        export DATA="${DATAROOT}/${jobid}.${fcsthr}"
        "${HOMEgfs}/jobs/JGFS_ATMOS_AWIPS_20KM_1P0DEG"
      fi
    fi
done

exit 0
