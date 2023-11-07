#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
    echo "Loading modules quietly..."
    set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"
if [[ "${MACHINE_ID}" != "noaacloud" ]]; then
  module use "${HOMEgfs}/sorc/ufs_model.fd/tests"
  module load modules.ufs_model.lua
  if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
    module load prod_util
    module load cray-pals
    module load cfp
    module load libjpeg
    module load craype-network-ucx
    module load cray-mpich-ucx
  else
    module load prod-util
    export UTILROOT=${prod_util_ROOT}
  fi
  module load wgrib2
  export WGRIB2=wgrib2
fi
if [[ "${MACHINE_ID}" = "hera" ]]; then
  module use "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/modulefiles/core"
  module load "miniconda3/4.6.14"
  module load "gfs_workflow/1.0.0"
fi
if [[ "${MACHINE_ID}" == "noaacloud" ]]; then
   if [[ "${PW_CSP:-}" = "aws" ]]; then

      # TODO: This can be cleaned-up; most of this is a hack for now.
      module use "/contrib/spack-stack/envs/ufswm/install/modulefiles/Core"
      module load "stack-intel"
      module load "stack-intel-oneapi-mpi"
      module use -a "/contrib/spack-stack/miniconda/modulefiles/miniconda/"
      module load "py39_4.12.0"
      module load "ufs-weather-model-env/1.0.0"
      export NETCDF="/contrib/spack-stack/miniconda/apps/miniconda/py39_4.12.0"
      # TODO: Are there plans for EPIC to maintain this package or should GW provide support?
      export UTILROOT="/contrib/global-workflow/NCEPLIBS-prod_util"
      export PATH="${PATH}:/contrib/global-workflow/bin"
      ndate_path="$(command -v ndate)"
      export NDATE="${ndate_path}"
   fi
fi

module list
unset MACHINE_ID

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
ftype=$(type -t set_trace)
if [[ "${ftype}" == "function" ]]; then
  set_trace
fi
