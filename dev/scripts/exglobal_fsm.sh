#!/bin/bash
set -x

# File Service Manager (FSM) for Global Workfow
# WGF (Workflow Group Family Assignment) - atmos, ocean
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
        # Initialize search array
        for fhr in $(seq 0 3 384); do
            array_element_atmos_master[fhr]="NO"
            array_element_ocean_uglo_15km[fhr]="NO"
            array_element_atm_log[fhr]="NO"
        done
        for fhr in $(seq 6 6 384); do
            array_element_ocean_6hr_avg[fhr]="NO"
            array_element_ice_6hr_avg[fhr]="NO"
        done
    fi
    if [[ "${RUN}" == "gdas" ]]; then
        scan_release_gdas_atmos_product="YES"
        scan_release_gdas_wave_post_gridded="YES"
        for fhr in $(seq 0 9); do
            array_element_atmos_master[fhr]="NO"
            array_element_ocean_uglo_15km[fhr]="NO"
        done
    fi
fi

#### COMINobsproc=${COMINobsproc:-${DMPDIR}/gfs.${PDY}/${cyc}/atmos}
COMINgdasobs="${COMINgdasobs:-${DMPDIR}/gdas.${previous_cycle_PDY}/${previous_cycle_cyc}/atmos}"

proceed_trigger_scan="YES"
while [[ "${proceed_trigger_scan}" == "YES" ]]; do
    proceed_trigger_scan="NO"

    #### release_gfs_atmos_prep
    if [[ "${scan_release_gfs_atmos_prep}" == "YES" ]]; then
        echo "Proceeding with scan_release_gfs_atmos_prep"
        COMINobsproc=${DMPDIR}/gfs.${PDY}/${cyc}/atmos
        # TODO: try to remove the use of ls.
        # shellcheck disable=SC2012
        if [[ -s "${COMINgdasobs}/gdas.t${previous_cycle_cyc}z.updated.status.tm00.bufr_d" ]] && [[ -s "${COMINobsproc}/gfs.t${cyc}z.prepbufr" ]] && [[ $(ls "${COMINobsproc}"/gfs.t*z.*.bufr_d | wc -l) -ge 60 ]]; then
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
        COMIN_prep_ocean_obs=${DMPDIR_ocean}/gfs.${PDY}/${cyc}/ocean
        for ty_md in adt icec sst insitu; do
            # Check for the existence of files for each type of marine observation; if any type is missing, skip the rest and wait for the next scan
            tty_files=("${COMIN_prep_ocean_obs}/${ty_md}"/*"${ty_md}"*)
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
        skip_this_scan="NO"
        scan_release_gfs_atmos_product="NO"
        echo "Proceeding with scan_release_gfs_atmos_product"
        fhr_max=384
        fhr=0
        while [[ "${fhr}" -le "${fhr_max}" ]]; do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            atmos_master=${COMIN_ATMOS_MASTER}/gfs.t${cyc}z.master.f${fhr_3d}.grib2
            if [[ "${array_element_atmos_master[fhr]}" == "YES" ]]; then
                # If this FHR is already found and event released
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    # All previous FHR were found; no need to skip
                    # Increase I/O performance by avoid redundant file search
                    if [[ -s "${atmos_master}" ]]; then
                        # Check for the file and set ecflow event as needed
                        release_event="YES"
                        array_element_atmos_master[10#${fhr}]="YES"
                        ecflow_client --event release_gfs_atmos_product_f"${fhr_3d}"
                    fi
                fi
            fi
            if [[ "${skip_this_scan}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${array_element_atmos_master[fhr]}" == "NO" ]]; then
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
            ocean_uglo_15km=${COMIN_WAVE_HISTORY}/gfs.t${cyc}z.uglo_15km.f${fhr_3d}.bin
            if [[ ${array_element_ocean_uglo_15km[fhr]} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${ocean_uglo_15km}" ]]; then
                        release_event="YES"
                        array_element_ocean_uglo_15km[10#${fhr}]="YES"
                        ecflow_client --event release_gfs_wave_post_gridded_f"${fhr_3d}"
                    fi
                fi
            fi
            if [[ "${skip_this_scan}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${array_element_ocean_uglo_15km[fhr]}" == "NO" ]]; then
                echo "FSM release_gfs_wave_post_gridded is waiting for file: ${ocean_uglo_15km}"
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
        TARGET_SIZE=957349888
        for fhr in $(seq 6 6 384); do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            ocean_6hr_avg=${COMIN_OCEAN_HISTORY}/gfs.t${cyc}z.6hr_avg.f${fhr_3d}.nc
            file_exist="NO"
            if [[ ${array_element_ocean_6hr_avg[fhr]} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${ocean_6hr_avg}" ]]; then
                        ACTUAL_SIZE=$(stat -c%s "${ocean_6hr_avg}")
                        if [[ "${ACTUAL_SIZE}" -ge "${TARGET_SIZE}" ]]; then
                            file_exist="YES"
                            release_event="YES"
                            array_element_ocean_6hr_avg[10#${fhr}]="YES"
                            ecflow_client --event release_gfs_ocean_product_f"${fhr_3d}"
                        fi
                    fi
                fi
            fi
            if [[ "${file_exist}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${skip_this_scan}" == "NO" ]] && [[ "${array_element_ocean_6hr_avg[fhr]}" == "NO" ]]; then
                echo "FSM release_gfs_ocean_product is waiting for file: ${ocean_6hr_avg}"
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
            ice_6hr_avg=${COMIN_ICE_HISTORY}/gfs.t${cyc}z.6hr_avg.f${fhr_3d}.nc
            if [[ ${array_element_ice_6hr_avg[fhr]} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${ice_6hr_avg}" ]]; then
                        release_event="YES"
                        array_element_ice_6hr_avg[10#${fhr}]="YES"
                        ecflow_client --event release_gfs_ice_product_f"${fhr_3d}"
                    fi
                fi
            fi
            if [[ ${skip_this_scan} == "NO" ]] && [[ ${release_event} == "NO" ]] && [[ ${array_element_ice_6hr_avg[fhr]} == "NO" ]]; then
                echo "FSM release_gfs_ice_product is waiting for file: ${ice_6hr_avg}"
                skip_this_scan="YES"
                scan_release_gfs_ice_product="YES"
                proceed_trigger_scan="YES"
            fi
        done
    fi

    #### release_gdas_atmos_prep
    if [[ "${scan_release_gdas_atmos_prep}" == "YES" ]]; then
        echo "Proceeding with scan_release_gdas_atmos_prep"
        COMINobsproc=${DMPDIR}/gdas.${PDY}/${cyc}/atmos
        if [[ -s ${COMINgdasobs}/gdas.t${previous_cycle_cyc}z.updated.status.tm00.bufr_d ]] && [[ -s ${COMINobsproc}/gdas.t${cyc}z.prepbufr ]] && [[ -s ${COMINobsproc}/gdas.t${cyc}z.updated.status.tm00.bufr_d ]]; then
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
        COMIN_prep_ocean_obs=${DMPDIR_ocean}/gdas.${PDY}/${cyc}/ocean
        for ty_md in adt icec sst insitu; do
            # TODO: try to remove the use of ls.
            # shellcheck disable=SC2012
            if [[ $(ls "${COMIN_prep_ocean_obs}"/"${ty_md}"/*"${ty_md}"* | wc -l) -eq 0 ]]; then
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
        skip_this_scan="NO"
        scan_release_gdas_atmos_product="NO"
        echo "Proceeding with scan_release_gdas_atmos_product"
        for fhr in $(seq 0 9); do
            release_event="NO"
            fhr_3d=$(printf "%03d" "${fhr}")
            atmos_master=${COMIN_ATMOS_MASTER}/gdas.t${cyc}z.master.f${fhr_3d}.grib2
            if [[ "${array_element_atmos_master[fhr]}" == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${atmos_master}" ]]; then
                        release_event="YES"
                        array_element_atmos_master[10#${fhr}]="YES"
                        ecflow_client --event release_gdas_atmos_product_f"${fhr_3d}"
                    fi
                fi
            fi
            if [[ "${skip_this_scan}" == "NO" ]] && [[ "${release_event}" == "NO" ]] && [[ "${array_element_atmos_master[fhr]}" == "NO" ]]; then
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
            ocean_uglo_15km=${COMIN_WAVE_HISTORY}/gdas.t${cyc}z.uglo_15km.f${fhr_3d}.bin
            if [[ ${array_element_ocean_uglo_15km[fhr]} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ "${skip_this_scan}" == "NO" ]]; then
                    if [[ -s "${ocean_uglo_15km}" ]]; then
                        release_event="YES"
                        array_element_ocean_uglo_15km[10#${fhr}]="YES"
                        ecflow_client --event release_gdas_wave_post_gridded_f"${fhr_3d}"
                    fi
                fi
            fi
            if [[ ${skip_this_scan} == "NO" ]] && [[ ${release_event} == "NO" ]] && [[ ${array_element_ocean_uglo_15km[fhr]} == "NO" ]]; then
                echo "FSM release_gdas_wave_post_gridded is waiting for file: ${ocean_uglo_15km}"
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
            if [[ ${array_element_atm_log[fhr]} == "YES" ]]; then
                echo "Skip found FHR${fhr_3d}"
            else
                if [[ ${skip_this_scan} == "NO" ]]; then
                    if [[ -s ${atm_log} ]]; then
                        release_event="YES"
                        array_element_atm_log[fhr]="YES"
                        ecflow_client --event "release_gfs_atmos_goesupp_f${fhr_3d}"
                    fi
                fi
            fi
            if [[ ${skip_this_scan} == "NO" ]] && [[ ${release_event} == "NO" ]] && [[ ${array_element_atm_log[fhr]} == "NO" ]]; then
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
