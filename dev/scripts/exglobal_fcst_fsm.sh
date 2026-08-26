#!/usr/bin/env bash
set -x
set -u -o pipefail
shopt -s nullglob

# File Service Manager (FSM) for Global Workfow
# WGF (Workflow Group Family Assignment) - atmos, wave, ocean, ice
# RJN (Request Job Name) - forecast

# Set default pgm for err_exit
pgm=$(basename "${BASH_SOURCE[0]}")
export pgm

# Initialize switch
scan_release_gfs_atmos_product="NO"
scan_release_gfs_ocean_product="NO"
scan_release_gfs_ice_product="NO"
scan_release_gdas_atmos_product="NO"
scan_release_gfs_wave_post_gridded="NO"
scan_release_gdas_wave_post_gridded="NO"
scan_release_gfs_atmos_goesupp="NO"

# Initialize sleep time for as long as 5 hours
sleep_time_interval=1

if [[ "${RJN}" == "forecast" ]]; then
    if [[ "${RUN}" == "gfs" ]]; then
        scan_release_gfs_atmos_product="YES"
        scan_release_gfs_wave_post_gridded="YES"
        scan_release_gfs_ocean_product="YES"
        scan_release_gfs_ice_product="YES"
        scan_release_gfs_atmos_goesupp="YES"
        # State arrays passed by name to scan_and_release (nameref indirect access).
        # shellcheck disable=SC2034
        declare -a atmos_master_product_ready
        # shellcheck disable=SC2034
        declare -a wave_uglo_15km_product_ready
        # shellcheck disable=SC2034
        declare -a atm_history_product_ready
        # shellcheck disable=SC2034
        declare -a ocean_6hr_avg_product_ready
        # shellcheck disable=SC2034
        declare -a ice_6hr_avg_product_ready
    fi
    if [[ "${RUN}" == "gdas" ]]; then
        scan_release_gdas_atmos_product="YES"
        scan_release_gdas_wave_post_gridded="YES"
        # shellcheck disable=SC2034
        declare -a atmos_master_product_ready
        # shellcheck disable=SC2034
        declare -a wave_uglo_15km_product_ready
    fi
fi

# ---------------------------------------------------------------------------
# scan_and_release: shared per-forecast-hour scan/release loop.
#
# For each forecast hour in the supplied list, verifies every required output
# file exists and is non-empty; when they do it marks the hour done in the
# product's state array and fires the matching ecflow_client --event release.
# On the first hour whose file(s) are still missing it stops releasing
# (downstream hours must not be released out of order), re-arms the product's
# scan flag, and requests another scan pass via proceed_trigger_scan.
#
# Usage:
#   scan_and_release <scan_flag_var> <state_array_name> <event_prefix> \
#                    <file_template(s)> <fhr>...
#
#   <scan_flag_var>    - name of the scan_release_* flag variable (nameref)
#   <state_array_name> - name of the *_product_ready state array (nameref)
#   <event_prefix>     - ecflow event prefix; event fired is <prefix>_fHHH
#   <file_template(s)> - one string with one or more space-separated COM path
#                        templates containing "%s" where the 3-digit forecast
#                        hour is substituted.  All files must be present for
#                        the hour to release.
#   <fhr>...           - forecast hours to scan, in ascending order
#
# shellcheck disable=SC2059 # templates intentionally carry %s for the fhr
scan_and_release() {
    # shellcheck disable=SC2034  # scan_flag & state are namerefs; assignments propagate to caller
    local flag_var="${1}" arr_name="${2}" event="${3}" tmpl="${4}"
    shift 4
    local -n scan_flag="${flag_var}"
    local -n state="${arr_name}"

    # tmpl may hold one or more space-separated templates; split into an array.
    local -a tmpls
    read -r -a tmpls <<< "${tmpl}"

    local skip="NO" fhr fhr_3d file first_file present t
    scan_flag="NO"
    for fhr in "$@"; do
        fhr_3d=$(printf "%03d" "${fhr}")
        # Already released on an earlier pass.
        [[ "${state[fhr]:-NO}" == "YES" ]] && {
            echo "Skip found FHR${fhr_3d}"
            continue
        }
        # An earlier hour this pass is still waiting; do not release out of order.
        [[ "${skip}" == "YES" ]] && continue

        # Require every template's file to exist and be non-empty for this hour.
        present="YES"
        first_file=""
        for t in "${tmpls[@]}"; do
            printf -v file "${t}" "${fhr_3d}"
            [[ -z "${first_file}" ]] && first_file="${file}"
            [[ -s "${file}" ]] || {
                present="NO"
                break
            }
        done

        if [[ "${present}" == "YES" ]]; then
            state[fhr]="YES"
            if [[ "${SENDECF}" == "YES" ]]; then
                ecflow_client --event "${event}_f${fhr_3d}"
            fi
        else
            echo "FSM ${event} is waiting for file: ${first_file}"
            skip="YES"
            scan_flag="YES"
            proceed_trigger_scan="YES"
        fi
    done
}

# Forecast-hour lists shared by the per-product scan/release calls below.
#   fhr_list_gfs  - gfs long-range: hourly to f120, 3-hourly to f384
#   fhr_list_6hr  - gfs ocean/ice 6-hour-average: f006..f384 by 6
#   fhr_list_gdas - gdas short-range: f000..f009 hourly
readarray -t fhr_list_gfs < <(
    seq 0 1 119
    seq 120 3 384
)
readarray -t fhr_list_6hr < <(seq 6 6 384)
readarray -t fhr_list_gdas < <(seq 0 9)

proceed_trigger_scan="YES"
while [[ "${proceed_trigger_scan}" == "YES" ]]; do
    proceed_trigger_scan="NO"

    #### release_gfs_atmos_product
    if [[ "${scan_release_gfs_atmos_product}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_atmos_product"
        # TODO: Use the atmos product logs. See issue 5130 (https://github.com/NOAA-EMC/global-workflow/issues/5130).
        scan_and_release scan_release_gfs_atmos_product atmos_master_product_ready \
            release_gfs_atmos_products \
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
            release_gfs_ocean_products \
            "${COMIN_OCEAN_HISTORY}/gfs.t${cyc}z.6hr_avg.log.f%s.txt" \
            "${fhr_list_6hr[@]}"
    fi

    #### release_gfs_ice_product
    if [[ "${scan_release_gfs_ice_product}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_ice_product"
        # Check if the copy-complete log file is present instead of the file itself.
        scan_and_release scan_release_gfs_ice_product ice_6hr_avg_product_ready \
            release_gfs_ice_products \
            "${COMIN_ICE_HISTORY}/gfs.t${cyc}z.log.ice.f%s.txt" \
            "${fhr_list_6hr[@]}"
    fi

    #### release_gdas_atmos_product
    if [[ "${scan_release_gdas_atmos_product}" == "YES" ]]; then
        echo "Proceeding with scan_release_gdas_atmos_product"
        scan_and_release scan_release_gdas_atmos_product atmos_master_product_ready \
            release_gdas_atmos_products \
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
    if [[ "${scan_release_gfs_atmos_goesupp}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_atmos_goesupp"
        scan_and_release scan_release_gfs_atmos_goesupp atm_history_product_ready \
            release_gfs_atmos_upp_goes \
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
