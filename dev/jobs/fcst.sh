#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
# TODO clean this up once ncdiag/1.1.2 is installed on WCOSS2
source "${HOMEgfs}/ush/detect_machine.sh"
if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
  source "${HOMEgfs}/dev/ush/load_ufswm_modules.sh"
else
  #TODO: Replace with gw_run modules when the weather model updates to spack-stack 1.9.2
  source "${HOMEgfs}/dev/ush/load_ufswm_modules.sh"
  module load py-netcdf4
  module load py-pyyaml
  module load py-jinja2
fi
status=$?
if [[ ${status} -ne 0 ]]; then
  exit "${status}"
fi

export job="fcst"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_FORECAST"
status=$?

exit "${status}"
