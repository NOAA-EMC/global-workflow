#! /usr/bin/env bash

. /apps/lmod/lmod/init/sh
source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

# TODO: clean this up
source "${HOMEgfs}/ush/detect_machine.sh"
#set +x # TEST --
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/sorc/ufs_model.fd/tests"
module load modules.ufs_model.lua
module load prod_util
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
  module load cray-pals
fi
module list
unset MACHINE_ID
set_trace

export job="gfsinit"
#export jobid="${job}.$$"
export jobid=


###############################################################
# Execute the JJOB
$(which sh) ${HOMEgfs}/jobs/JGLOBAL_GFS_INITIALIZE
status=$?


exit ${status}
