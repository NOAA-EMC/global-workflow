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

proceed_trigger_scan="YES"
while [[ "${proceed_trigger_scan}" == "YES" ]]; do
    proceed_trigger_scan="NO"

    #### release_gfs_atmos_product
    if [[ "${scan_release_gfs_atmos_product}" == "YES" ]]; then
        skip_this_scan="NO"
        scan_release_gfs_atmos_product="NO"
        echo "Proceeding with scan_release_gfs_atmos_product"
        fhr_max=384
        fhr=0
        while [[ "${fhr}" -le "${fhr_max}" ]]; do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            # TODO: Use the atmos product logs. See issue 5130 (https://github.com/NOAA-EMC/global-workflow/issues/5130).
            atmos_master=${COMIN_ATMOS_MASTER}/gfs.t${cyc}z.master.f${fhr_3d}.grib2
            atmos_sflux=${COMIN_ATMOS_MASTER}/gfs.t${cyc}z.sflux.f${fhr_3d}.grib2
            if [[ "${atmos_master_product_ready[fhr]:-NO}" == "YES" ]]; then
                # If this FHR is already found and event released
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    # All previous FHR were found; no need to skip
                    # Increase I/O performance by avoiding redundant file searches
                    if [[ -s "${atmos_master}" && -s "${atmos_sflux}" ]]; then
                        # Check for the file and set ecflow event as needed
                        release_event="YES"
                        atmos_master_product_ready[fhr]="YES"
                        if [[ "${SENDECF}" == "YES" ]]; then
                            ecflow_client --event release_gfs_atmos_product_f"${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ "${skip_this_scan}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${atmos_master_product_ready[fhr]:-NO}" == "NO" ]]; then
                echo "FSM release_gfs_atmos_product is waiting for file: ${atmos_master}"
                skip_this_scan="YES"
                scan_release_gfs_atmos_product="YES"
                proceed_trigger_scan="YES"
            fi
            if [[ "${fhr}" -lt 120 ]]; then
                fhr=$((fhr + 1))
            else
                fhr=$((fhr + 3))
            fi
            [[ ${skip_this_scan} == "YES" ]] && fhr=$((fhr_max + 1))
        done
    fi

    #### release_gfs_wave_post_gridded
    if [[ "${scan_release_gfs_wave_post_gridded}" == "YES" ]]; then
        skip_this_scan="NO"
        scan_release_gfs_wave_post_gridded="NO"
        echo "Proceeding with scan_release_gfs_wave_post_gridded"
        fhr_max=384
        fhr=0
        while [[ "${fhr}" -le "${fhr_max}" ]]; do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            # Check if the copy-complete log file is present instead of the file itself.
            wave_uglo_15km_log=${COMIN_WAVE_HISTORY}/gfs.t${cyc}z.uglo_15km.f${fhr_3d}.log
            if [[ ${wave_uglo_15km_product_ready[fhr]:-NO} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${wave_uglo_15km_log}" ]]; then
                        release_event="YES"
                        wave_uglo_15km_product_ready[fhr]="YES"
                        if [[ "${SENDECF}" == "YES" ]]; then
                            ecflow_client --event release_gfs_wave_post_gridded_f"${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ "${skip_this_scan}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${wave_uglo_15km_product_ready[fhr]:-NO}" == "NO" ]]; then
                echo "FSM release_gfs_wave_post_gridded is waiting for file: ${wave_uglo_15km_log}"
                skip_this_scan="YES"
                scan_release_gfs_wave_post_gridded="YES"
                proceed_trigger_scan="YES"
            fi
            if [[ "${fhr}" -lt 120 ]]; then
                fhr=$((fhr + 1))
            else
                fhr=$((fhr + 3))
            fi
            [[ ${skip_this_scan} == "YES" ]] && fhr=$((fhr_max + 1))
        done
    fi

    #### release_gfs_ocean_product
    if [[ "${scan_release_gfs_ocean_product}" == "YES" ]]; then
        skip_this_scan="NO"
        scan_release_gfs_ocean_product="NO"
        echo "Proceeding with scan_release_gfs_ocean_product"
        for fhr in $(seq 6 6 384); do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            # Check if the copy-complete log file is present instead of the file itself.
            ocean_6hr_avg_log=${COMIN_OCEAN_HISTORY}/gfs.t${cyc}z.6hr_avg.log.f${fhr_3d}.txt
            file_exist="NO"
            if [[ ${ocean_6hr_avg_product_ready[fhr]:-NO} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${ocean_6hr_avg_log}" ]]; then
                        file_exist="YES"
                        release_event="YES"
                        ocean_6hr_avg_product_ready[fhr]="YES"
                        if [[ "${SENDECF}" == "YES" ]]; then
                            ecflow_client --event release_gfs_ocean_product_f"${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ "${file_exist}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${skip_this_scan}" == "NO" ]] && [[ "${ocean_6hr_avg_product_ready[fhr]:-NO}" == "NO" ]]; then
                echo "FSM release_gfs_ocean_product is waiting for file: ${ocean_6hr_avg_log}"
                skip_this_scan="YES"
                scan_release_gfs_ocean_product="YES"
                proceed_trigger_scan="YES"
            fi
        done
    fi

    #### release_gfs_ice_product
    if [[ "${scan_release_gfs_ice_product}" == "YES" ]]; then
        skip_this_scan="NO"
        scan_release_gfs_ice_product="NO"
        echo "Proceeding with scan_release_gfs_ice_product"
        for fhr in $(seq 6 6 384); do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            # Check if the copy-complete log file is present instead of the file itself.
            ice_6hr_avg_log=${COMIN_ICE_HISTORY}/gfs.t${cyc}z.log.ice.f${fhr_3d}.txt
            if [[ ${ice_6hr_avg_product_ready[fhr]:-NO} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${ice_6hr_avg_log}" ]]; then
                        release_event="YES"
                        ice_6hr_avg_product_ready[fhr]="YES"
                        if [[ "${SENDECF}" == "YES" ]]; then
                            ecflow_client --event release_gfs_ice_product_f"${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ ${skip_this_scan} == "NO" ]] && [[ ${release_event} == "NO" ]] && [[ ${ice_6hr_avg_product_ready[fhr]:-NO} == "NO" ]]; then
                echo "FSM release_gfs_ice_product is waiting for file: ${ice_6hr_avg_log}"
                skip_this_scan="YES"
                scan_release_gfs_ice_product="YES"
                proceed_trigger_scan="YES"
            fi
        done
    fi

    #### release_gdas_atmos_product
    if [[ "${scan_release_gdas_atmos_product}" == "YES" ]]; then
        skip_this_scan="NO"
        scan_release_gdas_atmos_product="NO"
        echo "Proceeding with scan_release_gdas_atmos_product"
        for fhr in $(seq 0 9); do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            atmos_master=${COMIN_ATMOS_MASTER}/gdas.t${cyc}z.master.f${fhr_3d}.grib2
            atmos_sflux=${COMIN_ATMOS_MASTER}/gdas.t${cyc}z.sflux.f${fhr_3d}.grib2
            if [[ "${atmos_master_product_ready[fhr]:-NO}" == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${atmos_master}" && -s "${atmos_sflux}" ]]; then
                        release_event="YES"
                        atmos_master_product_ready[fhr]="YES"
                        if [[ "${SENDECF}" == "YES" ]]; then
                            ecflow_client --event release_gdas_atmos_product_f"${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ "${skip_this_scan}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${atmos_master_product_ready[fhr]:-NO}" == "NO" ]]; then
                echo "FSM release_gdas_atmos_product is waiting for file: ${atmos_master}"
                skip_this_scan="YES"
                scan_release_gdas_atmos_product="YES"
                proceed_trigger_scan="YES"
            fi
        done
    fi

    #### release_gdas_wave_post_gridded
    if [[ "${scan_release_gdas_wave_post_gridded}" == "YES" ]]; then
        skip_this_scan="NO"
        scan_release_gdas_wave_post_gridded="NO"
        echo "Proceeding with scan_release_gdas_wave_post_gridded"
        for fhr in $(seq 0 9); do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            # Check if the copy-complete log file is present instead of the file itself.
            wave_uglo_15km_log=${COMIN_WAVE_HISTORY}/gdas.t${cyc}z.uglo_15km.f${fhr_3d}.log
            if [[ ${wave_uglo_15km_product_ready[fhr]:-NO} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${wave_uglo_15km_log}" ]]; then
                        release_event="YES"
                        wave_uglo_15km_product_ready[fhr]="YES"
                        if [[ "${SENDECF}" == "YES" ]]; then
                            ecflow_client --event release_gdas_wave_post_gridded_f"${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ ${skip_this_scan} == "NO" ]] && [[ ${release_event} == "NO" ]] && [[ ${wave_uglo_15km_product_ready[fhr]:-NO} == "NO" ]]; then
                echo "FSM release_gdas_wave_post_gridded is waiting for file: ${wave_uglo_15km_log}"
                skip_this_scan="YES"
                scan_release_gdas_wave_post_gridded="YES"
                proceed_trigger_scan="YES"
            fi
        done
    fi

    #### release_gfs_atmos_goesupp
    if [[ ${scan_release_gfs_atmos_goesupp} == "YES" ]]; then
        skip_this_scan="NO"
        scan_release_gfs_atmos_goesupp="NO"
        echo "Proceeding with scan_release_gfs_atmos_goesupp"
        fhr_max=384
        fhr=0
        while [[ ${fhr} -le ${fhr_max} ]]; do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            atm_log=${COMIN_ATMOS_HISTORY}/gfs.t${cyc}z.log.f${fhr_3d}.txt
            if [[ ${atm_history_product_ready[fhr]:-NO} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ ${skip_this_scan} == "NO" ]]; then
                    if [[ -s ${atm_log} ]]; then
                        release_event="YES"
                        atm_history_product_ready[fhr]="YES"
                        if [[ "${SENDECF}" == "YES" ]]; then
                            ecflow_client --event "release_gfs_atmos_goesupp_f${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ ${skip_this_scan} == "NO" ]] && [[ ${release_event} == "NO" ]] && [[ ${atm_history_product_ready[fhr]:-NO} == "NO" ]]; then
                echo "FSM release_gfs_atmos_goesupp is waiting for file: ${atm_log}"
                skip_this_scan="YES"
                scan_release_gfs_atmos_goesupp="YES"
                proceed_trigger_scan="YES"
            fi
            if [[ ${fhr} -lt 120 ]]; then
                fhr=$((fhr + 1))
            else
                fhr=$((fhr + 3))
            fi
            [[ ${skip_this_scan} == "YES" ]] && fhr=$((fhr_max + 1))
        done
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
