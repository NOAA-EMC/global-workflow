#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## Offline UPP driver script
## UPP_RUN: analysis, forecast, goes, wafs.  See upp.yaml for valid options
## FHRLST : forecast hourlist to be post-process (e.g. f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
#. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
#status=$?
#if (( status != 0 )); then exit "${status}"; fi
# Temporarily load modules from UPP on WCOSS2
source "${HOMEgfs}/ush/detect_machine.sh"
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
  set +x
  source "${HOMEgfs}/ush/module-setup.sh"
  module use "${HOMEgfs}/sorc/ufs_model.fd/FV3/upp/modulefiles"
  module load "${MACHINE_ID}"
  module load prod_util
  module load cray-pals
  module load cfp
  module load libjpeg
  module load grib_util/1.2.3
  module load wgrib2/2.0.8
  export WGRIB2=wgrib2
  module load python/3.8.6
  module load crtm/2.4.0  # TODO: This is only needed when UPP_RUN=goes.  Is there a better way to handle this?
  set_trace

  # Add wxflow to PYTHONPATH
  wxflowPATH="${HOMEgfs}/ush/python"
  PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush:${wxflowPATH}"
  export PYTHONPATH

else
  # For spack-stack 1.6.0, the UPP has its own environment (upp-addon-dev)
  source "${HOMEgfs}/ush/module-setup.sh"
  module use "${HOMEgfs}/sorc/ufs_model.fd/FV3/upp/modulefiles"
  module load "${MACHINE_ID}"
  source "${HOMEgfs}/versions/run.spack.ver"
  module load "prod_util/${prod_util_ver}"
  module load "grib-util/${grib_util_ver}"
  # The numpy version in the UPP environment differs from the gsi-addon
  module load "py-numpy/1.22.3"
  module load "py-pyyaml/${py_pyyaml_ver}"
  module load "py-netcdf4/${py_netcdf4_ver}"
  module load "py-f90nml/${py_f90nml_ver}"

  # The jinja2 version in the UPP environment differs from the gsi-addon
  module load "py-jinja2/3.0.3"

  # xarray is not in this environment...
  # module load "py-xarray/${py_xarray_ver}"

  wxflowPATH="${HOMEgfs}/ush/python"
  PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush:${wxflowPATH}"
  export PYTHONPATH
  #. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"

  status=$?
  if (( status != 0 )); then exit "${status}"; fi
fi

export job="upp"
export jobid="${job}.$$"

export FORECAST_HOUR=$(( 10#${FHR3} ))

###############################################################
# Execute the JJOB
###############################################################
"${HOMEgfs}/jobs/JGLOBAL_ATMOS_UPP"

exit $?
