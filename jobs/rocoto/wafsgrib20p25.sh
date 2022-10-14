#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

export job="wafsgrib20p25"
export jobid="${job}.$$"

###############################################################
# TODO: sourcing configs should be in the j-job
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsgrib20p25"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

# TODO: missing sourcing $MACHINE.env

###############################################################
echo
echo "=============== START TO RUN WAFSGRIB20p25 ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGFS_ATMOS_WAFS_GRIB2_0P25
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################

exit 0
