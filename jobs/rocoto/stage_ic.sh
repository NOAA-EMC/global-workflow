#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

# Source FV3GFS workflow modules
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
[[ "${status}" -ne 0 ]] && exit "${status}"

export job="stage_ic"
export jobid="${job}.$$"

# Execute the JJOB
<<<<<<< HEAD
"${HOMEgfs}"/jobs/JGLOBAL_STAGE_IC
=======
"${HOMEgfs}/jobs/JGLOBAL_STAGE_IC"
>>>>>>> 1842c22f4f4afe945e32382dec6f48a2b2830123
status=$?


exit "${status}"
