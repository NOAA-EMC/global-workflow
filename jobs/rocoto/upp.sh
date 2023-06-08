#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## Offline UPP driver script
## UPP_RUN: analysis, forecast, goes, wafs.  See upp.yaml for valid options
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################
# setup python path for workflow utilities and tasks
pygwPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/pygw/src"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${pygwPATH}"
export PYTHONPATH

export job="upp"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
if [[ "${UPP_RUN}" = "analysis" ]]; then
    unset FHRLST
    FHRLST="f000"
fi
fhrlst=$(echo ${FHRLST} | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

for fhr in ${fhrlst}; do
    export FORECAST_HOUR=${fhr}
    ${HOMEgfs}/jobs/JGLOBAL_ATMOS_UPP
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

exit 0
