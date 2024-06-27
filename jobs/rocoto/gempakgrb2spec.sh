#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempakpgrb2spec"
export jobid="${job}.$$"

source "${HOMEgfs}/ush/jjob_header.sh" -e "gempak_spec" -c "base"

# shellcheck disable=SC2153
fhr3="${FHR3}"
fhr=$(( 10#${fhr3} ))
if (( fhr > FHMAX_GFS )); then
  echo "Nothing to process for FHR = ${fhr3}, cycle"
  continue
fi

export fcsthrs="${fhr3}"

# Execute the JJOB
"${HOMEgfs}/jobs/JGFS_ATMOS_GEMPAK_PGRB2_SPEC"

status=$?
exit "${status}"
