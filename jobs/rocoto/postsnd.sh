#! /usr/bin/env bash

PREAMBLE_SCRIPT="${PREAMBLE_SCRIPT:-$HOMEgfs/ush/preamble.sh}"
if [ -f "${PREAMBLE_SCRIPT}" ]; then
  source $PREAMBLE_SCRIPT
fi

###############################################################
# Source FV3GFS workflow modules

. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status


###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGFS_ATMOS_POSTSND
status=$?


exit $status

