#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="wafsgcip"
export jobid="${job}.$$"

#
###############################################################
# TODO: sourcing configs should be in the j-job
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsgcip"
for config in ${configs}; do
    . ${EXPDIR}/config.${config}
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

##########################################
# Source machine runtime environment
##########################################
. ${HOMEgfs}/env/${machine}.env wafsgcip
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################

###############################################################
echo
echo "=============== START TO RUN WAFSGCIP ==============="
# Execute the JJOB
${HOMEgfs}/jobs/JGFS_ATMOS_WAFS_GCIP
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################

exit 0
