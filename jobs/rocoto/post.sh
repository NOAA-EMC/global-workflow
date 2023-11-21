#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## NCEP post driver script
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="post"
export jobid="${job}.$$"

fhrlst=$(echo ${FHRLST} | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

#---------------------------------------------------------------
for fhr in ${fhrlst}; do
    export post_times=${fhr}
    ${HOMEgfs}/jobs/JGLOBAL_ATMOS_POST
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

exit 0
