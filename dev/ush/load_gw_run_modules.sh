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

case "${MACHINE_ID}" in
  container)
    source /usr/lmod/lmod/init/bash
    ;;
  *)
    # source /apps/lmod/lmod/init/bash
    ;;
esac

module purge
module use "${HOMEgfs}/sorc/ufs_model.fd/modulefiles"
module load "ufs_${MACHINE_ID}.intel"

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
# Set up the PYTHONPATH to include wxflow from HOMEgfs
if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
  PYTHONPATH="${HOMEgfs}/sorc/wxflow/src${PYTHONPATH:+:${PYTHONPATH}}"
fi
export PYTHONPATH

source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# Source versions file for runtime
source "${HOMEgfs}/versions/run.ver"

# Load our modules:
module use "${HOMEgfs}/modulefiles"

case "${MACHINE_ID}" in
  "wcoss2")
    module load cray-pals
    module load cfp
    module load libjpeg
    module load craype-network-ucx
    module load cray-mpich-ucx
    module load "gw_run.${MACHINE_ID}"
    ;;
  "hera" | "orion" | "hercules" | "gaeac5" | "gaeac6" | "noaacloud" | "ursa")
    module load "gw_run.${MACHINE_ID}"
    export UTILROOT=${prod_util_ROOT}
    ;;
  "container")
    source /usr/lmod/lmod/init/bash
    module load "gw_run.${MACHINE_ID}"
    export UTILROOT=${prod_util_ROOT}
    ;;
  *)
    echo "WARNING: UNKNOWN PLATFORM"
    ;;
esac

export err=$?
if [[ ${err} -ne 0 ]]; then
  echo "FATAL ERROR: Failed to load gw_run.${MACHINE_ID}"
  exit 1
fi

# module load wgrib2
# module load prod_util
export WGRIB2=wgrib2

# Turn on our settings
export SHELLOPTS
declare -xf set_strict
declare -xf set_trace
declare -xf postamble
declare -xf err_exit
set_strict
set_trace

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s
