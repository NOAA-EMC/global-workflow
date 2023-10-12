#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
#status=$?
#[[ ${status} -ne 0 ]] && exit ${status}

# TODO: clean this up
source "${HOMEgfs}/ush/detect_machine.sh"
set +x
source "${HOMEgfs}/ush/module-setup.sh"

if [[ "${MACHINE_ID}" != "noaacloud" ]]; then
  module use "${HOMEgfs}/sorc/ufs_model.fd/tests"
  module load modules.ufs_model.lua
  if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
    module load prod_util
    module load cray-pals
    module load libjpeg
  else
    module load prod-util
  fi
fi




if [[ "${MACHINE_ID}" = "hera" ]]; then
  module use "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/modulefiles/core"
  module load "miniconda3/4.6.14"
  module load "gfs_workflow/1.0.0"
# TODO: orion and wcoss2 will be uncommented when they are ready.  This comment block will be removed in the next PR
#elif [[ "${MACHINE_ID}" = "orion" ]]; then
#  module use "/home/rmahajan/opt/global-workflow/modulefiles/core"
#  module load "python/3.7.5"
#  module load "gfs_workflow/1.0.0"
#elif [[ "${MACHINE_ID}" = "wcoss2" ]]; then
#  module load "python/3.7.5"
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
set_trace





export job="wavepostpnt"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE_POST_PNT ==============="
# Execute the JJOB
${HOMEgfs}/jobs/JGLOBAL_WAVE_POST_PNT
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

exit 0
