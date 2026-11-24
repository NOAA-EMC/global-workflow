#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_common.sh
#
#   DESCRIPTION: Functions shared by all Unified Forecast System (UFS) components.
#                Includes date/time utilities, common variable definitions made
#                before run type determination (common_predet), the run type
#                determination (UFS_det; Cold Start, Warm Start, or Re-run), and
#                output handling for coupler-wide ESMF profiling (CPL_out).
#                Component-specific functions are defined in forecast_<component>.sh.

to_seconds() {
    # Function to convert HHMMSS to seconds since 00Z
    local hhmmss hh mm ss seconds padded_seconds
    hhmmss=${1:?}
    hh=${hhmmss:0:2}
    mm=${hhmmss:2:2}
    ss=${hhmmss:4:2}
    seconds=$((10#${hh} * 3600 + 10#${mm} * 60 + 10#${ss}))
    padded_seconds=$(printf "%05d" "${seconds}")
    echo "${padded_seconds}"
}

middle_date() {
    # Function to calculate mid-point date in YYYYMMDDHH between two dates also in YYYYMMDDHH
    local date1 date2 date1s date2s dtsecsby2 mid_date
    date1=${1:?}
    date2=${2:?}
    date1s=$(date --utc -d "${date1:0:8} ${date1:8:2}:00:00" +%s)
    date2s=$(date --utc -d "${date2:0:8} ${date2:8:2}:00:00" +%s)
    dtsecsby2=$(($((date2s - date1s)) / 2))
    mid_date=$(date --utc -d "${date1:0:8} ${date1:8:2} + ${dtsecsby2} seconds" +%Y%m%d%H%M%S)
    echo "${mid_date:0:10}"
}

nhour() {
    # Function to calculate hours between two dates (This replicates prod-util NHOUR)
    local date1 date2 seconds1 seconds2 hours
    date1=${1:?}
    date2=${2:?}
    # Convert dates to UNIX timestamps
    seconds1=$(date --utc -d "${date1:0:8} ${date1:8:2}:00:00" +%s)
    seconds2=$(date --utc -d "${date2:0:8} ${date2:8:2}:00:00" +%s)
    hours=$(($((seconds1 - seconds2)) / 3600)) # Calculate the difference in seconds and convert to hours
    echo "${hours}"
}

# shellcheck disable=SC2034
common_predet() {
    echo "SUB ${FUNCNAME[0]}: Defining variables for shared through model components"

    RUN=${RUN:-gdas}
    rCDUMP=${rCDUMP:-${RUN}}

    ENSMEM=${ENSMEM:-000}
    MEMBER=$(( ENSMEM < 0 ? -10#${ENSMEM#-} : 10#${ENSMEM} )) # -1: control, 0: ensemble mean, >0: ensemble member $MEMBER

    # Define significant cycles
    half_window=$((assim_freq / 2))
    current_cycle="${PDY}${cyc}"
    previous_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} - ${assim_freq} hours" +%Y%m%d%H)
    next_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${assim_freq} hours" +%Y%m%d%H)
    current_cycle_begin=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} - ${half_window} hours" +%Y%m%d%H)
    current_cycle_end=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${half_window} hours" +%Y%m%d%H)
    next_cycle_begin=$(date --utc -d "${next_cycle:0:8} ${next_cycle:8:2} - ${half_window} hours" +%Y%m%d%H)
    forecast_end_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${FHMAX} hours" +%Y%m%d%H)

    # Define model start date for current_cycle and next_cycle as the time the forecast will start
    if [[ "${DOIAU:-NO}" == "YES" ]]; then
        model_start_date_current_cycle="${current_cycle_begin}"
        model_start_date_next_cycle="${next_cycle_begin}"
    else
        model_start_date_current_cycle=${current_cycle}
        if [[ "${DO_AERO_ANL:-NO}" == "YES" ]]; then
            # even without IAU we want 3-hourly restarts for FGAT
            model_start_date_next_cycle="${next_cycle_begin}"
        else
            model_start_date_next_cycle=${next_cycle}
        fi
    fi

    FHMIN=${FHMIN:-0}
    FHMAX=${FHMAX:-9}
    FHOUT=${FHOUT:-3}
    FHMAX_HF=${FHMAX_HF:-0}
    FHOUT_HF=${FHOUT_HF:-1}

    mkdir -p "${COMOUT_CONF}"

    cd "${DATA}" && true
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "FATAL ERROR: Unable to 'cd ${DATA}', ABORT!"
        exit 1
    fi

    # Several model components share DATA/INPUT for input data
    mkdir -p "${DATA}/INPUT"

}

# Disable variable not used warnings
# shellcheck disable=SC2034
UFS_det() {
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

        # Check if we are doing a cold start IAU
        if [[ ${DOIAU_COLDSTART:-"NO"} == "NO" ]]; then
            DOIAU="NO"
            IAU_OFFSET=0
            model_start_date_current_cycle=${current_cycle}
            DO_LAND_IAU=".false."
        else
            IAU_OFFSET=${IAU_OFFSET:-0}
        fi

        # It is still possible that a restart is available from a previous forecast attempt
        # So we have to continue checking for restarts
    fi

    # Lets assume this is was not run before and hence this is not a RERUN
    RERUN="NO"

    # RERUN is only available for RUN=gfs|gefs. It is not available for RUN=gdas|enkfgdas|enkfgfs
    if [[ "${RUN}" =~ "gdas" ]] || [[ "${RUN}" == "enkfgfs" ]]; then
        echo "RERUN is not available for RUN='${RUN}'"
        return 0
    fi

    # However, if this was run before, a DATArestart/FV3_RESTART must exist with data in it.
    local file_array nrestarts
    mapfile -t file_array < <(find "${DATArestart}/FV3_RESTART" -name "????????.??0000.coupler.res" | sort)
    nrestarts=${#file_array[@]}
    if [[ ${nrestarts} -eq 0 ]]; then
        echo "No restarts found in '${DATArestart}/FV3_RESTART', RERUN='${RERUN}'"
        return 0
    else
        echo "Found ${nrestarts} restarts in '${DATArestart}/FV3_RESTART' to check for RERUN"
        ls -1 "${DATArestart}/FV3_RESTART/"????????.??0000.coupler.res
    fi

    # Look in reverse order of file_array to determine available restart times
    # Each coupled component checks for its own restarts via <COMPONENT>_det
    local ii filepath filename rdate rst_ok
    for ((ii = nrestarts - 1; ii >= 0; ii--)); do

        filepath="${file_array[ii]}"
        filename=$(basename "${filepath}")     # Strip path from YYYYMMDD.HH0000.coupler.res
        rdate="${filename:0:8}${filename:9:2}" # match YYYYMMDD and HH of YYYYMMDD.HH0000.coupler.res

        # Assume all is well; all restarts are available
        rst_ok="YES"

        if ! FV3_det "${rdate}"; then
            rst_ok="NO"
        fi

        if [[ "${cplflx}" == ".true." ]]; then
            if ! CMEPS_det "${rdate}"; then
                rst_ok="NO"
            fi
            if ! MOM6_det "${rdate}"; then
                rst_ok="NO"
            fi
        fi

        if [[ "${cplice}" == ".true." ]]; then
            if ! CICE_det "${rdate}"; then
                rst_ok="NO"
            fi
        fi

        if [[ "${cplwav}" == ".true." ]]; then
            if ! WW3_det "${rdate}"; then
                rst_ok="NO"
            fi
        fi

        # Collective check
        if [[ "${rst_ok}" == "YES" ]]; then
            RERUN="YES"
            RERUN_DATE="${rdate}"

            # Check if RERUN_DATE is at/after model end time; if so, this will cause the model to crash
            if [[ "${RERUN_DATE}" -ge "${forecast_end_cycle}" ]]; then
                echo "FATAL ERROR Warm start detected, but restart date (${RERUN_DATE}) is at/after model end date (${forecast_end_cycle})"
                exit 1
            fi

            warm_start=".true."
            echo "All restarts found for '${RERUN_DATE}', RERUN='${RERUN}', warm_start='${warm_start}'"
            break
        fi

    done # loop over nrestarts
}

CPL_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for general cpl fields"
    if [[ "${esmf_profile:-.false.}" == ".true." ]]; then
        if [[ ! -d "${COMOUT_ATMOS_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_ATMOS_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_ATMOS_HISTORY}"
        fi
        cpfs "${DATA}/ESMF_Profile.summary" "${COMOUT_ATMOS_HISTORY}/ESMF_Profile.summary"
    fi
}
