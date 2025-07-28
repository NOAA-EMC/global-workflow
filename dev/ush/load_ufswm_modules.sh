#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
  echo "Loading modules quietly..."
  set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$(ulimit -S -s)

source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

module use "${HOMEgfs}/sorc/ufs_model.fd/modulefiles"
module load "ufs_${MACHINE_ID}.intel"
export err=$?
if [[ ${err} -ne 0 ]]; then
  echo "FATAL ERROR: Failed to load ufs_${MACHINE_ID}.intel"
  exit 1
fi
module load prod_util
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
  module load cray-pals
  module load cfp
  module load libjpeg
  module load craype-network-ucx
  module load cray-mpich-ucx
  module load python/3.8.6
else
  export UTILROOT=${prod_util_ROOT}
fi
module load wgrib2
export WGRIB2=wgrib2

module list
unset MACHINE_ID

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

# If this function exists in the environment, run it; else do not
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
fi
