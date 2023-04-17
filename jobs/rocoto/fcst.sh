#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

# TODO: clean this up
source "${HOMEgfs}/ush/detect_machine.sh"
set +x
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/sorc/ufs_model.fd/tests"
module load modules.ufs_model.lua
module load prod_util
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
  module load cray-pals
fi
if [[ "${MACHINE_ID}" = "hera" ]]; then
  module use "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/modulefiles/core"
  module load "miniconda3/4.6.14"
  module load "gfs_workflow/1.0.0"
# TODO: orion and wcoss2 will be uncommented when they are ready.  This comment block will be removed in the next PR
#elif [[ "${MACHINE_ID}" = "orion" ]]; then
#  module use "/home/rmahajan/opt/global-workflow/modulefiles/core"
#  module load "python/3.7.5"
#  module load "gfs_workflow/1.0.0"
#elif [[ "${MACHINE_ID}" = "wcoss2" ]]; then
#  module load "python/3.7.5"
fi
module list
unset MACHINE_ID
set_trace

###############################################################
# exglobal_forecast.py requires the following in PYTHONPATH
# This will be moved to a module load when ready
pygwPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/pygw/src:${HOMEgfs}/ush/python/pygfs"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${pygwPATH}"
export PYTHONPATH

export job="fcst"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGLOBAL_FORECAST
status=$?


exit ${status}
