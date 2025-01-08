#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## atmosphere products driver script
## FHRLST : forecast hour list to post-process (e.g. -f001, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="atmos_ensstat"

# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FORECAST_HOUR jobid
for FORECAST_HOUR in "${fhr_list[@]}"; do
	fhr3=$(printf '%03d' "${FORECAST_HOUR}")
	jobid="${job}_f${fhr3}.$$"
	###############################################################
	# Execute the JJOB
	###############################################################
	"${HOMEgfs}/jobs/JGLOBAL_ATMOS_ENSSTAT"
	status=$?
	[[ ${status} -ne 0 ]] && exit "${status}"
done

exit 0
