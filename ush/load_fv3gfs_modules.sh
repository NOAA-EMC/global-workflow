#! /usr/bin/env bash

###############################################################
if [[ "$-" == *x* ]]; then
  set_x=YES
else
  set_x=NO
fi

if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
  echo "Loading modules quietly..."
  set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$(ulimit -S -s)

# Find module command and purge:
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# Source versions file for runtime
source "${HOMEgfs}/versions/run.ver"

# Load our modules:
module use "${HOMEgfs}/modulefiles"

case "${MACHINE_ID}" in
"wcoss2" | "hera" | "orion" | "hercules" | "gaeac5" | "gaeac6" | "noaacloud")
  module load "module_base.${MACHINE_ID}"
  export err=$?
  if [[ ${err} -ne 0 ]]; then
    echo "FATAL ERROR: Failed to load module_base.${MACHINE_ID}"
    exit 1
  fi
  ;;
*)
  echo "WARNING: UNKNOWN PLATFORM"
  ;;
esac

module list

# If this function exists in the environment, run it; else set -x if it was set on entering this script
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
elif [[ "${set_x}" == "YES" ]]; then
  set -x
fi

# Set up the PYTHONPATH to include wxflow from HOMEgfs
if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
  PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/sorc/wxflow/src"
fi

# Add HOMEgfs/ush/python to PYTHONPATH
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush/python"
export PYTHONPATH

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s
