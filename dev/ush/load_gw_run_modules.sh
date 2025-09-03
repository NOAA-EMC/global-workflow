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
ulimit_s=$( ulimit -S -s )

source "${HOMEgfs}/ush/preamble.sh"

# Find module command and purge:
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

case "$${MACHINE_ID}" in
  container)
    source /usr/lmod/lmod/init/bash
    ;;
  *)
    source /apps/lmod/lmod/init/bash
    ;;
esac

module purge
module use ${HOMEgfs}/sorc/ufs_model.fd/modulefiles
module load ufs_${MACHINE_ID}.intel

# If this function exists in the environment, run it; else set -x if it was set on entering this script
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
elif [[ "${set_x}" == "YES" ]]; then
  set -x
fi

# Add wxflow to PYTHONPATH
wxflowPATH="${HOMEgfs}/ush/python"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush:${wxflowPATH}"
export PYTHONPATH

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s
