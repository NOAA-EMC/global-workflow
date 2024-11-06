#! /usr/bin/env bash

# Disable variable not used warnings
# shellcheck disable=SC2034
UFS_det(){
  echo "SUB ${FUNCNAME[0]}: Run type determination for UFS"

  # Determine if the current cycle is a warm start (based on the availability of restarts)
  if [[ -f "${COMIN_ATMOS_RESTART_PREV}/${model_start_date_current_cycle:0:8}.${model_start_date_current_cycle:8:2}0000.coupler.res" ]]; then
    warm_start=".true."
  fi 

  # If restarts were not available, this is likely a cold start
  if [[ "${warm_start}" == ".false." ]]; then

    # Since restarts are not available from the previous cycle, this is likely a cold start
    # Ensure cold start ICs are present when warm start is not set
    # TODO: add checks for other cold start ICs as well
    if [[ ! -f "${COMIN_ATMOS_INPUT}/gfs_ctrl.nc" ]]; then
      echo "FATAL ERROR: Cold start ICs are missing from '${COMIN_ATMOS_INPUT}'"
      exit 1
    fi

    # Since warm start is false, we cannot do IAU
    DOIAU="NO"
    IAU_OFFSET=0
    model_start_date_current_cycle=${current_cycle}

    # It is still possible that a restart is available from a previous forecast attempt
    # So we have to continue checking for restarts
  fi

  # Lets assume this is was not run before and hence this is not a RERUN
  RERUN="NO"

  # RERUN is only available for RUN=gfs|gefs It is not available for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" ]] || [[ "${RUN}" == "enkfgfs" ]]; then
    echo "RERUN is not available for RUN='${RUN}'"
    return 0
  fi

  # However, if this was run before, a DATArestart/FV3_RESTART must exist with data in it.
  local file_array nrestarts
  # shellcheck disable=SC2312
  mapfile -t file_array < <(find "${DATArestart}/FV3_RESTART" -name "????????.??0000.coupler.res" | sort)
  nrestarts=${#file_array[@]}
  if (( nrestarts == 0 )); then
    echo "No restarts found in '${DATArestart}/FV3_RESTART', RERUN='${RERUN}'"
    return 0
  fi

  # Look in reverse order of file_array to determine available restart times
  local ii filepath filename
  local rdate seconds
  local fv3_rst_ok cmeps_rst_ok mom6_rst_ok cice6_rst_ok ww3_rst_ok
  for (( ii=nrestarts-1; ii>=0; ii-- )); do

    filepath="${file_array[ii]}"
    filename=$(basename "${filepath}")  # Strip path from YYYYMMDD.HH0000.coupler.res
    rdate="${filename:0:8}${filename:9:2}"  # match YYYYMMDD and HH of YYYYMMDD.HH0000.coupler.res

    # Assume all is well; all restarts are available
    fv3_rst_ok="YES"
    cmeps_rst_ok="YES"
    mom6_rst_ok="YES"
    cice6_rst_ok="YES"
    ww3_rst_ok="YES"

    # Check for FV3 restart availability
    if [[ ! -f "${DATArestart}/FV3_RESTART/${rdate:0:8}.${rdate:8:2}0000.coupler.res" ]]; then
    # TODO: add checks for other FV3 restarts as well
      fv3_rst_ok="NO"
    fi

    # Check for CMEPS and MOM6 restart availability
    if [[ "${cplflx}" == ".true." ]]; then
      seconds=$(to_seconds "${rdate:8:2}0000")
      if [[ ! -f "${DATArestart}/CMEPS_RESTART/ufs.cpld.cpl.r.${rdate:0:4}-${rdate:4:2}-${rdate:6:2}-${seconds}.nc" ]]; then
        cmeps_rst_ok="NO"
      fi
      if [[ ! -f "${DATArestart}/MOM6_RESTART/${rdate:0:8}.${rdate:8:2}0000.MOM.res.nc" ]]; then
      # TODO: add checks for other MOM6 restarts as well
        mom6_rst_ok="NO"
      fi
    fi

    # Check for CICE6 restart availability
    if [[ "${cplice}" == ".true." ]]; then
      if [[ ! -f "${DATArestart}/CICE_RESTART/cice_model.res.${rdate:0:4}-${rdate:4:2}-${rdate:6:2}-${seconds}.nc" ]]; then
        cice6_rst_ok="NO"
      fi
    fi

    # Check for WW3 restart availability
    if [[ "${cplwav}" == ".true." ]]; then
      if [[ ! -f "${DATArestart}/WW3_RESTART/${rdate:0:8}.${rdate:8:2}0000.restart.ww3" ]]; then
        ww3_rst_ok="NO"
      fi
    fi

    # Collective check
    if [[ "${fv3_rst_ok}" == "YES" ]] \
      && [[ "${cmeps_rst_ok}" == "YES" ]] \
      && [[ "${mom6_rst_ok}" == "YES" ]] \
      && [[ "${cice6_rst_ok}" == "YES" ]] \
      && [[ "${ww3_rst_ok}" == "YES" ]]; then
      RERUN="YES"
      RERUN_DATE="${rdate}"
      warm_start=".true."
      echo "All restarts found for '${RERUN_DATE}', RERUN='${RERUN}', warm_start='${warm_start}'"
      break
    fi

  done  # loop over nrestarts
}
