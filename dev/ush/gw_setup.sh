#! /bin/bash

#
# Resets the lmod environment and loads the modules necessary to run all the
#   scripts necessary to prepare the workflow for use (checkout, experiment
#   setup, etc.).
#
# This script should be SOURCED to properly setup the environment.
#

script_dir="$(cd "$(dirname  "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd )"
HOMEgfs=$(cd "${script_dir}" && git rev-parse --show-toplevel)
export HOMEgfs
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/modulefiles"
module load "gw_setup.${MACHINE_ID}"

unset HOMEgfs
