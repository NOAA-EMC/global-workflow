#! /usr/bin/env bash

# This script loads the UPP modules directly from the repository
# TODO: Remove this script when the UPP hash has been updated to support spack-stack 1.9.2
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

source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# The UPP module for C6 is simply "gaea.lua"
if [[ ${MACHINE_ID} == "gaeac6" ]]; then
   _machine=gaea
else
   _machine=${MACHINE_ID}
fi

module use "${HOMEgfs}/sorc/ufs_model.fd/FV3/upp/modulefiles"
module load "${_machine}"
module load prod_util
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
  module load cray-pals
  module load cfp
  module load libjpeg
  module load craype-network-ucx
  module load cray-mpich-ucx
else
  module load py-netcdf4
  module load py-jinja2
  module load py-pyyaml
  module load py-xarray
  module load py-f90nml
  module load grib-util
  export UTILROOT=${prod_util_ROOT}
fi
module load wgrib2
export WGRIB2=wgrib2

module list
unset MACHINE_ID _machine

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

