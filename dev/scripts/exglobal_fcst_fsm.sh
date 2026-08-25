#!/bin/bash
set -x
set -u -o pipefail
shopt -s nullglob

# File Service Manager (FSM) for Global Workfow
# WGF (Workflow Group Family Assignment) - atmos, wave, ocean, ice
# RJN (Request Job Name) - prep, forecast

# Set default pgm for err_exit
pgm=$(basename "${BASH_SOURCE[0]}")
export pgm

previous_cycle=$("${NDATE}" -6 "${PDY}${cyc}")
previous_cycle_PDY="${previous_cycle:0:8}"
previous_cycle_cyc="${previous_cycle:8:2}"

# Initialize switch
scan_release_gfs_atmos_prep="NO"
scan_release_gdas_atmos_prep="NO"
scan_release_gfs_marine_prepoceanobs="NO"
scan_release_gfs_atmos_product="NO"
scan_release_gfs_ocean_product="NO"
scan_release_gfs_ice_product="NO"
scan_release_gdas_atmos_product="NO"
scan_release_gfs_wave_post_gridded="NO"
scan_release_gdas_marine_prepoceanobs="NO"
scan_release_gdas_wave_post_gridded="NO"
scan_release_gfs_atmos_goesupp="NO"

# Initialize sleep time for as long as 5 hours
sleep_time_interval=1

if [[ "${RUN}" == "gfs" ]]; then
    if [[ "${RJN}" == "prep" ]]; then
        if [[ "${WGF}" == "atmos" ]]; then
            scan_release_gfs_atmos_prep="YES"
        elif [[ "${WGF}" == "marine" ]]; then
            scan_release_gfs_marine_prepoceanobs="YES"
        fi
    fi
fi

if [[ "${RUN}" == "gdas" ]]; then
    if [[ "${RJN}" == "prep" ]]; then
        if [[ "${WGF}" == "atmos" ]]; then
            scan_release_gdas_atmos_prep="YES"
        elif [[ "${WGF}" == "marine" ]]; then
            scan_release_gdas_marine_prepoceanobs="YES"
        fi
    fi
fi

if [[ "${RJN}" == "forecast" ]]; then
    if [[ "${RUN}" == "gfs" ]]; then
        scan_release_gfs_atmos_product="YES"
        scan_release_gfs_wave_post_gridded="YES"
        scan_release_gfs_ocean_product="YES"
        scan_release_gfs_ice_product="YES"
        scan_release_gfs_atmos_goesupp="YES"
        # Create search arrays (to be filled with "YES" when the target log file is found)
        declare -a atmos_master_product_ready
        declare -a wave_uglo_15km_product_ready
        declare -a atm_history_product_ready
        declare -a ocean_6hr_avg_product_ready
        declare -a ice_6hr_avg_product_ready
    fi
    if [[ "${RUN}" == "gdas" ]]; then
        scan_release_gdas_atmos_product="YES"
        scan_release_gdas_wave_post_gridded="YES"
        declare -a atmos_master_product_ready
        declare -a wave_uglo_15km_product_ready
    fi
fi

COMIN_ATMOS_OBS_gfs=${COMIN_ATMOS_OBS_gfs:-$(compath.py "${envir}/obsproc/${obsproc_ver}")/"gfs.${PDY}/${cyc}/atmos"}
COMIN_ATMOS_OBS_gdas=${COMIN_ATMOS_OBS_gdas:-$(compath.py "${envir}/obsproc/${obsproc_ver}")/"gdas.${PDY}/${cyc}/atmos"}
COMIN_ATMOS_OBS_PREV_gdas=${COMIN_ATMOS_OBS_PREV_gdas:-$(compath.py "${envir}/obsproc/${obsproc_ver}")/"gdas.${previous_cycle_PDY}/${previous_cycle_cyc}/atmos"}
COMIN_OCEAN_OBS_gfs=${COMIN_OCEAN_OBS_gfs:-"${ROTDIR}/gfs.${PDY}/${cyc}/obs"}
COMIN_OCEAN_OBS_gdas=${COMIN_OCEAN_OBS_gdas:-"${ROTDIR}/gdas.${PDY}/${cyc}/obs"}

# ---------------------------------------------------------------------------
# scan_and_release: shared per-forecast-hour scan/release loop (GH#5129).
#
# Collapses the seven previously duplicated per-product blocks into one place.
# Release behavior is unchanged: for each forecast hour in the supplied list it
# verifies every required output file exists and is non-empty; when they do it
# marks the hour done in the product's state array and fires the matching
# "ecflow_client --event <prefix>_fHHH" release. On the first hour whose file(s)
# are still missing it stops releasing (downstream hours must not be released
# out of order), re-arms the product's scan flag, and requests another scan
# pass via the shared proceed_trigger_scan global.
#
# Usage:
#   scan_and_release <scan_flag_var> <state_array_name> <event_prefix> \
#                    <file_template(s)> <fhr>...
#
#   <scan_flag_var>    - name of the scan_release_* flag variable (nameref)
#   <state_array_name> - name of the *_product_ready state array (nameref)
#   <event_prefix>     - ecflow event prefix; event fired is <prefix>_fHHH
#   <file_template(s)> - one string with one or more space-separated COM path
#                        templates, each containing "%s" where the 3-digit
#                        forecast hour is substituted. When more than one is
#                        given (e.g. master + sflux) every file must be present
#                        for the hour to release; the first names the "waiting"
#                        message.
#   <fhr>...           - forecast hours to scan, in ascending order
#
# shellcheck disable=SC2059  # templates intentionally carry a %s for the fhr
scan_and_release() {
    local flag_var="${1}" arr_name="${2}" event="${3}" tmpl="${4}"
    shift 4
    local -n scan_flag="${flag_var}"   # nameref to the scan_release_* flag
    local -n state="${arr_name}"       # nameref to the *_product_ready array

    # tmpl may hold one or more space-separated templates; split into an array.
    local -a tmpls
    read -r -a tmpls <<< "${tmpl}"

    local skip="NO" fhr fhr_3d file first_file present t
    scan_flag="NO"
    for fhr in "$@"; do
        fhr_3d=$(printf "%03d" "${fhr}")
        # Already released on an earlier pass.
        [[ "${state[fhr]:-NO}" == "YES" ]] && { echo "Skip found FHR${fhr_3d}"; continue; }
        # An earlier hour this pass is still waiting; do not release out of order.
        [[ "${skip}" == "YES" ]] && continue

        # Require every template's file to exist and be non-empty for this hour.
        present="YES"
        first_file=""
        for t in "${tmpls[@]}"; do
            printf -v file "${t}" "${fhr_3d}"
            [[ -z "${first_file}" ]] && first_file="${file}"
            [[ -s "${file}" ]] || { present="NO"; break; }
        done

        if [[ "${present}" == "YES" ]]; then
            state[fhr]="YES"
            ecflow_client --event "${event}_f${fhr_3d}"
        else
            echo "FSM ${event} is waiting for file: ${first_file}"
            skip="YES"
            scan_flag="YES"
            proceed_trigger_scan="YES"
        fi
    done
}

# Forecast-hour lists shared by the per-product scan/release calls below.
#   fhr_list_gfs  - gfs long-range products: hourly to f120, 3-hourly to f384
#   fhr_list_6hr  - gfs ocean/ice 6-hour-average products: f006..f384 by 6
#   fhr_list_gdas - gdas short-range products: f000..f009 hourly
readarray -t fhr_list_gfs < <(seq 0 1 119; seq 120 3 384)
readarray -t fhr_list_6hr < <(seq 6 6 384)
readarray -t fhr_list_gdas < <(seq 0 9)

proceed_trigger_scan="YES"
while [[ "${proceed_trigger_scan}" == "YES" ]]; do
    proceed_trigger_scan="NO"

    #### release_gfs_atmos_prep
    if [[ "${scan_release_gfs_atmos_prep}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_atmos_prep"
        # TODO: try to remove the use of ls.
        # shellcheck disable=SC2012
        if [[ -s "${COMIN_ATMOS_OBS_PREV_gdas}/gdas.t${previous_cycle_cyc}z.updated.status.tm00.bufr_d" ]] && [[ -s "${COMIN_ATMOS_OBS_gfs}/gfs.t${cyc}z.prepbufr" ]] && [[ $(ls "${COMIN_ATMOS_OBS_gfs}"/gfs.t*z.*.bufr_d | wc -l) -ge 60 ]]; then
            ecflow_client --event release_gfs_atmos_prep
            scan_release_gfs_atmos_prep="NO"
        else
            proceed_trigger_scan="YES"
        fi
    fi

    #### release_gfs_marine_prepoceanobs
    if [[ "${scan_release_gfs_marine_prepoceanobs}" == "YES" ]]; then
        skip_this_scan="YES"
        echo "Proceeding with scan_release_gfs_marine_prepoceanobs"
        # TODO remove/change this and look at obsproc for the bufr files
        for ty_md in adt icec sst; do
            # Check for the existence of files for each type of marine observation; if any type is missing, skip the rest and wait for the next scan
            if [[ ${ty_md} == "adt" && ${cyc} != "00" ]]; then
                echo "adt files are only produced at 00z...skipping check for ${cyc}z"
                continue
            else
                tty_files=("${COMIN_OCEAN_OBS_gfs}/"*"${ty_md}"*)
            fi
            count_tty=${#tty_files[@]}
            if [[ ${count_tty} -eq 0 ]]; then
                skip_this_scan="NO"
                proceed_trigger_scan="YES"
            fi
        done
        if [[ "${skip_this_scan}" == "YES" ]]; then
            ecflow_client --event release_gfs_marine_prepoceanobs
            scan_release_gfs_marine_prepoceanobs="NO"
        fi
    fi

    #### release_gfs_atmos_product
    if [[ "${scan_release_gfs_atmos_product}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_atmos_product"
        # TODO: Use the atmos product logs. See issue 5130 (https://github.com/NOAA-EMC/global-workflow/issues/5130).
        scan_and_release scan_release_gfs_atmos_product atmos_master_product_ready \
            release_gfs_atmos_product \
            "${COMIN_ATMOS_MASTER}/gfs.t${cyc}z.master.f%s.grib2 ${COMIN_ATMOS_MASTER}/gfs.t${cyc}z.sflux.f%s.grib2" \
            "${fhr_list_gfs[@]}"
    fi

    #### release_gfs_wave_post_gridded
    if [[ "${scan_release_gfs_wave_post_gridded}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_wave_post_gridded"
        # Check if the copy-complete log file is present instead of the file itself.
        scan_and_release scan_release_gfs_wave_post_gridded wave_uglo_15km_product_ready \
            release_gfs_wave_post_gridded \
            "${COMIN_WAVE_HISTORY}/gfs.t${cyc}z.uglo_15km.f%s.log" \
            "${fhr_list_gfs[@]}"
    fi

    #### release_gfs_ocean_product
    if [[ "${scan_release_gfs_ocean_product}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_ocean_product"
        # Check if the copy-complete log file is present instead of the file itself.
        scan_and_release scan_release_gfs_ocean_product ocean_6hr_avg_product_ready \
            release_gfs_ocean_product \
            "${COMIN_OCEAN_HISTORY}/gfs.t${cyc}z.6hr_avg.log.f%s.txt" \
            "${fhr_list_6hr[@]}"
    fi

    #### release_gfs_ice_product
    if [[ "${scan_release_gfs_ice_product}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_ice_product"
        # Check if the copy-complete log file is present instead of the file itself.
        scan_and_release scan_release_gfs_ice_product ice_6hr_avg_product_ready \
            release_gfs_ice_product \
            "${COMIN_ICE_HISTORY}/gfs.t${cyc}z.log.ice.f%s.txt" \
            "${fhr_list_6hr[@]}"
    fi

    #### release_gdas_atmos_prep
    if [[ "${scan_release_gdas_atmos_prep}" == "YES" ]]; then
        echo "Proceeding with scan_release_gdas_atmos_prep"
        if [[ -s ${COMIN_ATMOS_OBS_PREV_gdas}/gdas.t${previous_cycle_cyc}z.updated.status.tm00.bufr_d ]] && [[ -s ${COMIN_ATMOS_OBS_gdas}/gdas.t${cyc}z.prepbufr ]] && [[ -s ${COMIN_ATMOS_OBS_gdas}/gdas.t${cyc}z.updated.status.tm00.bufr_d ]]; then
            ecflow_client --event release_gdas_atmos_prep
            scan_release_gdas_atmos_prep="NO"
        else
            proceed_trigger_scan="YES"
        fi
    fi

    #### release_gdas_marine_prepoceanobs
    if [[ "${scan_release_gdas_marine_prepoceanobs}" == "YES" ]]; then
        skip_this_scan="YES"
        echo "Proceeding with scan_release_gdas_marine_prepoceanobs"
        # TODO remove/change this and look at obsproc for the bufr files
        for ty_md in adt icec sst; do
            if [[ ${ty_md} == "adt" && ${cyc} != "00" ]]; then
                echo "adt files are only produced at 00z...skipping check for ${cyc}z"
                continue
            else
                tty_files=("${COMIN_OCEAN_OBS_gfs}/"*"${ty_md}"*)
            fi
            # Check for the existence of files for each type of marine observation; if any type is missing, skip the rest and wait for the next scan
            count_tty=${#tty_files[@]}
            if [[ ${count_tty} -eq 0 ]]; then
                skip_this_scan="NO"
                proceed_trigger_scan="YES"
            fi
        done

        if [[ "${skip_this_scan}" == "YES" ]]; then
            ecflow_client --event release_gdas_marine_prepoceanobs
            scan_release_gdas_marine_prepoceanobs="NO"
        fi
    fi

    #### release_gdas_atmos_product
    if [[ "${scan_release_gdas_atmos_product}" == "YES" ]]; then
        echo "Proceeding with scan_release_gdas_atmos_product"
        scan_and_release scan_release_gdas_atmos_product atmos_master_product_ready \
            release_gdas_atmos_product \
            "${COMIN_ATMOS_MASTER}/gdas.t${cyc}z.master.f%s.grib2 ${COMIN_ATMOS_MASTER}/gdas.t${cyc}z.sflux.f%s.grib2" \
            "${fhr_list_gdas[@]}"
    fi

    #### release_gdas_wave_post_gridded
    if [[ "${scan_release_gdas_wave_post_gridded}" == "YES" ]]; then
        echo "Proceeding with scan_release_gdas_wave_post_gridded"
        # Check if the copy-complete log file is present instead of the file itself.
        scan_and_release scan_release_gdas_wave_post_gridded wave_uglo_15km_product_ready \
            release_gdas_wave_post_gridded \
            "${COMIN_WAVE_HISTORY}/gdas.t${cyc}z.uglo_15km.f%s.log" \
            "${fhr_list_gdas[@]}"
    fi

    #### release_gfs_atmos_goesupp
    if [[ ${scan_release_gfs_atmos_goesupp} == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_atmos_goesupp"
        scan_and_release scan_release_gfs_atmos_goesupp atm_history_product_ready \
            release_gfs_atmos_goesupp \
            "${COMIN_ATMOS_HISTORY}/gfs.t${cyc}z.log.f%s.txt" \
            "${fhr_list_gfs[@]}"
    fi
    sleep_time_interval=$((sleep_time_interval + 1))
    if [[ ${sleep_time_interval} -eq 600 ]]; then
        echo "Waiting over 5 hours for file not exist. Retry"
        export err=1
        err_chk
    fi
    [[ ${proceed_trigger_scan} == "YES" ]] && sleep 30
done # proceed_trigger_scan

exit 0
