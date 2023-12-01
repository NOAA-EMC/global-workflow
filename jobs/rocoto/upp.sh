#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## Offline UPP driver script
## UPP_RUN: analysis, forecast, goes, wafs.  See upp.yaml for valid options
## FHRLST : forecast hourlist to be post-process (e.g. f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
#. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
#status=$?
#if (( status != 0 )); then exit "${status}"; fi
# Temporarily load modules from UPP
source "${HOMEgfs}/ush/detect_machine.sh"
set +x
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/sorc/ufs_model.fd/FV3/upp/modulefiles"
module load "${MACHINE_ID}"
module load prod_util
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
    module load cray-pals
    module load cfp
    module load libjpeg
    module load grib_util
else
    # shellcheck disable=SC2154
    export UTILROOT="${prod_util_ROOT}"
    module load grib-util
    if [[ "${MACHINE_ID}" = "hera" ]]; then
        module use "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/hpc-stack/modulefiles/stack"
    elif [[ "${MACHINE_ID}" = "orion" ]]; then
        module use "/work2/noaa/global/wkolczyn/save/hpc-stack/modulefiles/stack"
    fi
    module load hpc/1.2.0
    module load hpc-miniconda3/4.6.14
    module load gfs_workflow/1.0.0
fi
module load wgrib2
export WGRIB2=wgrib2
module list
set_trace
# End hack

###############################################################
# setup python path for workflow utilities and tasks
wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/wxflow/src"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${wxflowPATH}"
export PYTHONPATH

export job="upp"
export jobid="${job}.$$"

###############################################################
# shellcheck disable=SC2153
fhrlst=$(echo "${FHRLST}" | sed -e 's/f//g')  # strip off the 'f' in the forecast hour list
IFS='_' read -ra fhrs <<< "${fhrlst}"  # convert to array

# Execute the JJOB
# shellcheck disable=SC2068
for fhr in ${fhrs[@]}; do
    export FORECAST_HOUR=$(( 10#${fhr} ))
    "${HOMEgfs}/jobs/JGLOBAL_ATMOS_UPP"
    status=$?
    if (( status != 0 )); then exit "${status}"; fi
done

exit 0
