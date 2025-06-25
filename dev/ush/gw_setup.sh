#! /bin/bash

#
# Resets the lmod environment and loads the modules necessary to run all the
#   scripts necessary to prepare the workflow for use (checkout, experiment
#   setup, etc.).
#
# This script should be SOURCED to properly setup the environment.
#

HOMEgfs="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd)"
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"

# Set up the PYTHONPATH for wxflow if it is not already loaded
if ! python3 -c "import wxflow" 2>/dev/null; then
  if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
    PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/sorc/wxflow/src"
    export PYTHONPATH
  fi
fi
