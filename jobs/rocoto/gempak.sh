#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempak"

# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FORECAST_HOUR jobid
for FORECAST_HOUR in "${fhr_list[@]}"; do
	fhr3=$(printf '%03d' "${FORECAST_HOUR}")
	jobid="${job}_f${fhr3}.$$"
	###############################################################
	# Execute the JJOB
	###############################################################
	"${HOMEgfs}/jobs/J${RUN^^}_ATMOS_GEMPAK"
	status=$?
	[[ ${status} -ne 0 ]] && exit "${status}"
done

exit 0
