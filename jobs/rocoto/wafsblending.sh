#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="wafsblending"
export jobid="${job}.$$"

###############################################################
# TODO: sourcing configs should be in the j-job
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsblending"
for config in ${configs}; do
    . ${EXPDIR}/config.${config}
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

# TODO: Mising source machine runtime environment

###############################################################

echo
echo "=============== START TO RUN WAFSBLENDING ==============="
# Execute the JJOB
${HOMEgfs}/jobs/JGFS_ATMOS_WAFS_BLENDING
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################

exit 0
