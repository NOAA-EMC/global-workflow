#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## atmosphere products driver script
## FHRLST : forecast hour list to post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="atmos_products"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
fhrlst=$(echo "${FHRLST}" | sed -e 's/f//g')
IFS='_' read -ra fhrs <<< "${fhrlst}"

#---------------------------------------------------------------
for fhr in ${fhrs[@]}; do
    export FORECAST_HOUR=$(( 10#${fhr} ))
    ${HOMEgfs}/jobs/JGLOBAL_ATMOS_PRODUCTS
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

exit 0
