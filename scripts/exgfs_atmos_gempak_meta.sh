#! /usr/bin/env bash

GEMGRD1="${RUN}_1p00_${PDY}${cyc}f"

export numproc=23

# Find the last hour available
for (( fhr = fhend; fhr >= fhbeg; fhr = fhr - fhinc )) ; do
  fhr3=$(printf "%03d" "${fhr}")
  if [[ -r "${COMIN_ATMOS_GEMPAK_1p00}/${GEMGRD1}${fhr3}" ]]; then
    break
  fi
done

sleep_interval=20
max_tries=180
first_time=0
do_all=0

#loop through and process needed forecast hours
while [[ ${fhr} -le ${fhend} ]]; do
  #
  # First check to see if this is a rerun.  If so make all Meta files
  if [[ ${fhr} -gt 126 && ${first_time} -eq 0 ]]; then
    do_all=1
  fi
  first_time=1

  if [[ ${fhr} -eq 120 ]]; then
    fhr=126
  fi

  gempak_file="${COMIN_ATMOS_GEMPAK_1p00}/${GEMGRD1}${fhr3}"
  if ! wait_for_file "${gempak_file}" "${sleep_interval}" "${max_tries}"; then
    err_exit "FATAL ERROR: gempak grid file ${gempak_file} not available after maximum wait time."
  fi

  export fhr

  ########################################################
  # Create a script to be poe'd
  #
  #  Note:  The number of scripts to be run MUST match the number
  #  of total_tasks set in the ecf script, or the job will fail.
  #
  rm -f poescript

  fhr3=$(printf "%03d" "${fhr}")

  if [[ ${do_all} -eq 1 ]] ; then
    do_all=0
    # shellcheck disable=SC2312
    awk '{print $1}' "${HOMEgfs}/gempak/fix/gfs_meta" | envsubst > "poescript"
  else
    #
    #    Do not try to grep out 12, it will grab the 12 from 126.
    #    This will work as long as we don't need 12 fhr metafiles
    #
    if [[ ${fhr} -ne 12 ]] ; then
      # shellcheck disable=SC2312
      grep "${fhr}" "${HOMEgfs}/gempak/fix/gfs_meta" | awk -F" [0-9]" '{print $1}' | envsubst > "poescript"
    fi
  fi

  #  If this is the final fcst hour, alert the
  #  file to all centers.
  #
  if [[ ${fhr} -ge ${fhend} ]] ; then
    export DBN_ALERT_TYPE=GFS_METAFILE_LAST
  fi

  export fend=${fhr}

  cat poescript

  "${HOMEgfs}/ush/run_mpmd.sh" poescript && true
  export err=$?
  if [[ ${err} -ne 0 ]]; then
     err_exit "Failed to generate one or more gempak meta plots!"
  fi

  if [[ ${fhr} -eq 126 ]] ; then
    fhr=$((fhr + 6))
  else
    fhr=$((fhr + fhinc))
  fi
done

exit
