#! /bin/bash

#
# Resets the lmod environment and loads the modules necessary to run all the
#   scripts necessary to prepare the workflow for use (checkout, experiment
#   setup, etc.).
#
# This script should be SOURCED to properly setup the environment.
#

# Determine if HOMEgfs is already set
unset_homegfs=NO
if [[ -z "${HOMEgfs+x}" ]]; then
  script_dir="$(cd "$(dirname  "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd )"
  HOMEgfs=$(cd "${script_dir}" && git rev-parse --show-toplevel)
  export HOMEgfs
  unset_homegfs=YES
fi
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

module use "${HOMEgfs}/modulefiles"
module load "gw_setup.${MACHINE_ID}"
err=$?
if [[ "${err}" -ne 0 ]]; then
  echo "FATAL ERROR: Failed to load module_gwsetup.${MACHINE_ID}"
  exit 1
fi

# Set up the PYTHONPATH to include wxflow from HOMEgfs
if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
  PYTHONPATH="${HOMEgfs}/sorc/wxflow/src${PYTHONPATH:+:${PYTHONPATH}}"
  export PYTHONPATH
fi


if [[ ${unset_homegfs} == "YES" ]]; then
  unset HOMEgfs
fi

