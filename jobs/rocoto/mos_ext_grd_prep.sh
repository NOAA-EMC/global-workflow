#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="mos_ext_grd_prep"
export jobid="${job}.$$"

###############################################################
# Source jjob_header before invoking external JJOB

source "${HOMEgfs}/ush/jjob_header.sh" -e "mos_ext_grd_prep" -c "base mos_ext_grd_prep"

###############################################################
# Execute the JJOB

"${HOMEgfs_mos}/jobs/JGFSMOS_EXT_GRD_PREP"
status=$?

exit "${status}"
