#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
    echo "Loading modules quietly..."
    set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# exglobal_forecast.py requires the following in PYTHONPATH
# This will be moved to a module load when ready
wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/wxflow/src:${HOMEgfs}/ush/python/pygfs"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${wxflowPATH}"
export PYTHONPATH

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

# If this function exists in the environment, run it; else do not
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
fi
