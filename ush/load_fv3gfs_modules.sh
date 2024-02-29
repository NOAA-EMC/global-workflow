#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
    echo "Loading modules quietly..."
    set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

# Find module command and purge:
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# Source versions file for runtime
source "${HOMEgfs}/versions/run.ver"

# Load our modules:
module use "${HOMEgfs}/modulefiles"

case "${MACHINE_ID}" in
  "wcoss2" | "hera" | "orion" | "hercules" | "gaea" | "jet" | "s4")
    module load "module_base.${MACHINE_ID}"
    ;;
  *)
    echo "WARNING: UNKNOWN PLATFORM"
    ;;
esac

module list

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

# If this function exists in the environment, run it; else do not
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
fi
