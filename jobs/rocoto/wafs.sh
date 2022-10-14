#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

export job="wafs"
export jobid="${job}.$$"

###############################################################
###############################################################
# TODO: sourcing configs should be in the j-job
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafs"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################

echo
echo "=============== START TO RUN WAFS ==============="

# Loop through fcsthrs
hr=0
while [ $hr -le 120 ]; do

  export fcsthrs=$(printf "%03d" $hr)

  # Execute the JJOB
  $HOMEgfs/jobs/JGFS_ATMOS_WAFS
  status=$?
  [[ $status -ne 0 ]] && exit $status

  hr=$(expr $hr + 6)

done

exit 0
