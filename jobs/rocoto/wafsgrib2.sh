#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

export job="wafsgrib2"
export jobid=${job}.$$

###############################################################
# TODO: Sourcing configs should be done in the j-job
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsgrib2"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

# TODO: Missing sourcing of $MACHINE.env

###############################################################

echo
echo "=============== START TO RUN WAFSGRIB2 ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGFS_ATMOS_WAFS_GRIB2
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################

exit 0
