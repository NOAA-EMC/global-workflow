#! /usr/bin/env bash

set -x

###############################################################
## Offline UPP driver script
## UPP_RUN: analysis, forecast, goes.  See upp.yaml for valid options
## FHRLST : forecast hourlist to be post-process (e.g. f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
#source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
#status=$?
#if (( status != 0 )); then exit "${status}"; fi
# Temporarily load modules from UPP on WCOSS2
source "${HOMEgfs}/ush/detect_machine.sh"
if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
  set +x
  source "${HOMEgfs}/ush/module-setup.sh"
  module use "${HOMEgfs}/sorc/ufs_model.fd/UFSATM/upp/modulefiles"
  module load "${MACHINE_ID}_intel"
  module load prod_util
  module load cray-pals
  module load cfp
  module load libjpeg
  module load grib_util/1.2.3
  module load wgrib2/2.0.8
  export WGRIB2=wgrib2
  module load python/3.8.6
  if [[ "${UPP_RUN:-}" == "goes" ]]; then
    module load crtm/2.4.0
  fi
  set -x

  # Set up the PYTHONPATH to include wxflow from HOMEgfs
  if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
    PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/sorc/wxflow/src"
  fi

  # Add HOMEgfs/ush/python to PYTHONPATH
  PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush/python"
  export PYTHONPATH

else
  source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
  status=$?
  if [[ ${status} -ne 0 ]]; then
     exit "${status}";
  fi
fi

export job="upp"
export jobid="${job}.$$"

export FORECAST_HOUR=$(( 10#${FHR3} ))

###############################################################
# Execute the JJOB
###############################################################
"${HOMEgfs}/jobs/JGLOBAL_ATMOS_UPP"

exit $?
