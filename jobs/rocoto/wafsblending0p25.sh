#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

export job="wafsblending0p25"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################
# TODO: sourcing configs should be in the j-job
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsblending0p25"
for config in ${configs}; do
    . ${EXPDIR}/config.${config}
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

# TODO: Mising source machine runtime environment

###############################################################

echo
echo "=============== START TO RUN WAFSBLENDING0P25 ==============="
# Execute the JJOB
${HOMEgfs}/jobs/JGFS_ATMOS_WAFS_BLENDING_0P25
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################

exit 0
