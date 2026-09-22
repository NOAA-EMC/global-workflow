#!/usr/bin/env bash

#
# Resets the lmod environment and loads the modules necessary to run all the
#   scripts necessary to prepare the workflow for use (checkout, experiment
#   setup, etc.).
#
# This script should be SOURCED to properly setup the environment.
#

# Determine if HOMEglobal is already set
unset_homegfs=NO
if [[ -z "${HOMEglobal:-}" ]]; then
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
    HOMEglobal=$(cd "${script_dir}" && git rev-parse --show-toplevel)
    export HOMEglobal
    unset_homegfs=YES
fi
source "${HOMEglobal}/ush/detect_machine.sh"
source "${HOMEglobal}/ush/module-setup.sh"

module use "${HOMEglobal}/modulefiles"
module load "gw_setup.${MACHINE_ID}"
err=$?
if [[ "${err}" -ne 0 ]]; then
    echo "FATAL ERROR: Failed to load module_gwsetup.${MACHINE_ID}"
    exit 1
fi

# Set up the PYTHONPATH to include wxflow from HOMEglobal
if [[ -d "${HOMEglobal}/sorc/wxflow/src" ]]; then
    PYTHONPATH="${HOMEglobal}/sorc/wxflow/src${PYTHONPATH:+:${PYTHONPATH}}"
    export PYTHONPATH
fi

# Source rocoto helper functions for use in the global-workflow
source "${HOMEglobal}/dev/ush/rocoto_helpers.sh"

if [[ ${unset_homegfs} == "YES" ]]; then
    unset HOMEglobal
fi
