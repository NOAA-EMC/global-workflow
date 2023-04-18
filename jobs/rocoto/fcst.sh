#! /usr/bin/env bash

#source "${HOMEgfs}/ush/preamble.sh" # HRW
source "${HOMEgfs}/ush/preamble.testing.sh" # HRW

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

# TODO: clean this up
#source "${HOMEgfs}/ush/detect_machine.sh" # HRW
#set +x # HRW
#source "${HOMEgfs}/ush/module-setup.sh" # HRW
#module use "${HOMEgfs}/sorc/ufs_model.fd/tests" # HRW
#module load modules.ufs_model.lua # HRW
#module load prod_util # HRW
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
  module load cray-pals
fi
#if [[ "${MACHINE_ID}" = "hera" ]]; then # HRW
#  module use "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/modulefiles/core" # HRW
#  module load "miniconda3/4.6.14" # HRW
#  module load "gfs_workflow/1.0.0" # HRW
# TODO: orion and wcoss2 will be uncommented when they are ready.  This comment block will be removed in the next PR
#elif [[ "${MACHINE_ID}" = "orion" ]]; then
#  module use "/home/rmahajan/opt/global-workflow/modulefiles/core"
#  module load "python/3.7.5"
#  module load "gfs_workflow/1.0.0"
#elif [[ "${MACHINE_ID}" = "wcoss2" ]]; then
#  module load "python/3.7.5"
#fi # HRW
#module list # HRW
# unset MACHINE_ID # HRW
# set_trace # HRW

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
