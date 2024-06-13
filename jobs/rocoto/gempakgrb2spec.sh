#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempakpgrb2spec"
export jobid="${job}.$$"

source "${HOMEgfs}/ush/jjob_header.sh" -e "gempak_spec" -c "base"

# shellcheck disable=SC2153
fhrlst=$(echo "${FHRLST}" | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

for fhr3 in ${fhrlst}; do
  fhr=$(( 10#${fhr3} ))
  if (( fhr > FHMAX_GFS )); then
    echo "Nothing to process for FHR = ${fhr3}, cycle"
    continue
  fi

  export fcsthrs="${fhr3}"

# Execute the JJOB
  "${HOMEgfs}/jobs/JGFS_ATMOS_GEMPAK_PGRB2_SPEC"
done

status=$?
exit "${status}"
