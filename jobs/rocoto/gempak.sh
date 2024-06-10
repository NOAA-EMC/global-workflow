#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="gempak"
export jobid="${job}.$$"

source "${HOMEgfs}/ush/jjob_header.sh" -e "gempak" -c "base gempak"

fhrlst=$(echo "${FHRLST}" | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

for fhr3 in ${fhrlst}; do
    fhr=$(( 10#${fhr3} ))
    if (( fhr > FHMAX_GFS )); then
	echo "Nothing to process for FHR = ${fhr3}, cycle"
	continue
    fi

    export fcsthrs="${fhr3}"

# Execute the JJOB
    "${HOMEgfs}/jobs/J${RUN^^}_ATMOS_GEMPAK"
done

status=$?
exit "${status}"
