#! /usr/bin/env bash

cd "${DATA}" || exit 1

# Set paramlist files based on FORECAST_HOUR (-1, 0, 3, 6, etc.)
# Determine if supplemental products (PGBS) (1-degree and 1/2-degree) should be generated
if [[ "${FORECAST_HOUR}" -le 0 ]]; then
    if [[ "${FORECAST_HOUR}" -lt 0 ]]; then
        fhr3="analysis"
        # shellcheck disable=SC2034  # paramlista is used later indirectly
        paramlista="${paramlista_anl}"
        FLXGF="NO"
    elif [[ "${FORECAST_HOUR}" -eq 0 ]]; then
        fhr3=$(printf "f%03d" "${FORECAST_HOUR}")
        # shellcheck disable=SC2034  # paramlista is used later indirectly
        paramlista="${paramlista_f000}"
    fi
    PGBS="YES"
else
    fhr3=$(printf "f%03d" "${FORECAST_HOUR}")
    if ((FORECAST_HOUR % FHOUT_PGBS == 0)); then
        PGBS="YES"
    fi
fi

#-----------------------------------------------------
# Section creating pressure grib2 interpolated products

# Determine grids once and save them as a string and an array for processing
grid_string="0p25"
if [[ "${PGBS:-}" == "YES" ]]; then
    grid_string="${grid_string}:0p50:1p00"
else
    echo "INFO: Supplemental product generation is disabled for fhr = ${fhr3}"
    PGBS="NO" # Can't generate supplemental products if PGBS is not YES
fi
# Also transform the ${grid_string} into an array for processing
IFS=':' read -ra grids <<< "${grid_string}"

# Files needed by ${USHgfs}/interp_atmos_master.sh
MASTER_FILE="${COMIN_ATMOS_MASTER}/${PREFIX}master.${fhr3}.grib2"

for ((nset = 1; nset <= downset; nset++)); do

    echo "INFO: Begin processing nset = ${nset}"

    # Each set represents a group of files
    if [[ "${nset}" == 1 ]]; then
        grp="a"
    elif [[ "${nset}" == 2 ]]; then
        grp="b"
    fi

    # Get inventory from ${MASTER_FILE} that matches patterns from ${paramlist}
    # Extract this inventory from ${MASTER_FILE} into a smaller tmpfile based on paramlist

    tmpfile="tmpfile${grp}_${fhr3}"
    paramlist="paramlist${grp}"
    parmfile="${!paramlist}"

    # shellcheck disable=SC2312
    ${WGRIB2} "${MASTER_FILE}" | grep -F -f "${parmfile}" | ${WGRIB2} -i -grib "${tmpfile}" "${MASTER_FILE}" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "FATAL ERROR: wgrib2 failed to create intermediate grib2 file from '${MASTER_FILE}' using '${parmfile}'"
    fi

    # Number of processors available to process $nset
    nproc=${ntasks}

    # shellcheck disable=SC2312
    ncount=$(${WGRIB2} "${tmpfile}" | wc -l)
    if [[ "${nproc}" -gt "${ncount}" ]]; then
        echo "WARNING: Total no. of available processors '${nproc}' exceeds no. of records '${ncount}' in ${tmpfile}"
        echo "WARNING: Reduce nproc to ${ncount} (or less) to not waste resources"
    fi

    inv=$((ncount / nproc))
    rm -f "${DATA}/cmdfile"

    last=0
    for ((iproc = 1; iproc <= nproc; iproc++)); do
        first=$((last + 1))
        last=$((last + inv))
        if [[ ${last} -gt ${ncount} ]]; then
            last=${ncount}
        fi

        # if final record of is u-component, add next record v-component
        # if final record is land, add next record icec
        # grep returns 1 if no match is found, so temporarily turn off exit on non-zero rc
        set +e
        # shellcheck disable=SC2312
        ${WGRIB2} -d "${last}" "${tmpfile}" | grep -E -i "ugrd|ustm|uflx|u-gwd|land|maxuw"
        rc=$?
        set_strict
        if [[ ${rc} == 0 ]]; then # Matched the grep
            last=$((last + 1))
        fi
        if [[ ${iproc} -eq ${nproc} || ${last} -gt ${ncount} ]]; then
            last=${ncount}
        fi

        # Break tmpfile into processor specific chunks in preparation for MPMD
        ${WGRIB2} "${tmpfile}" -for "${first}":"${last}" -grib "${tmpfile}_${iproc}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "wgrib2 failed to geneate an intermediate grib2 file from ${tmpfile} records ${first} to ${last}"
        fi
        input_file="${tmpfile}_${iproc}"
        output_file_prefix="pgb2${grp}file_${fhr3}_${iproc}"
        echo "${USHgfs}/interp_atmos_master.sh ${input_file} ${output_file_prefix} ${grid_string}" >> "${DATA}/cmdfile"

        # if at final record and have not reached the final processor then write echo's to
        # cmdfile for remaining processors
        if [[ "${last}" -eq "${ncount}" ]]; then
            for ((pproc = iproc + 1; pproc <= nproc; pproc++)); do
                echo "/bin/echo ${pproc}" >> "${DATA}/cmdfile"
            done
            break
        fi
    done # for (( iproc = 1 ; iproc <= nproc ; iproc++ )); do

    # Run with MPMD or serial
    "${USHgfs}/run_mpmd.sh" "${DATA}/cmdfile" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "FATAL ERROR: Some or all interpolations of the master grib file failed during MPMD execution!"
    fi

    # We are in a loop over downset, save output from mpmd into nset specific output
    mv mpmd.out "mpmd_${nset}.out"

    # Concatenate grib files from each processor into a single one
    # and clean-up as you go
    echo "INFO: Concatenating processor-specific grib2 files into a single product file"
    for ((iproc = 1; iproc <= nproc; iproc++)); do
        for grid in "${grids[@]}"; do
            if [[ -s "pgb2${grp}file_${fhr3}_${iproc}_${grid}" ]]; then
                cat "pgb2${grp}file_${fhr3}_${iproc}_${grid}" >> "pgb2${grp}file_${fhr3}_${grid}"
                rm -f "pgb2${grp}file_${fhr3}_${iproc}_${grid}"
            fi
        done
        # There is no further use of the processor specific tmpfile; delete it
        rm -f "${tmpfile}_${iproc}"
    done

    # Move to COM and index the product grib files
    for grid in "${grids[@]}"; do
        ${WGRIB2} -s "pgb2${grp}file_${fhr3}_${grid}" > "pgb2${grp}file_${fhr3}_${grid}.idx"
        prod_dir="COMOUT_ATMOS_GRIB_${grid}"
        cpfs "pgb2${grp}file_${fhr3}_${grid}" "${!prod_dir}/${PREFIX}pres_${grp}.${grid}.${fhr3}.grib2"
        cpfs "pgb2${grp}file_${fhr3}_${grid}.idx" "${!prod_dir}/${PREFIX}pres_${grp}.${grid}.${fhr3}.grib2.idx"
    done

    echo "INFO: Finished processing nset = ${nset}"

done # for (( nset=1 ; nset <= downset ; nset++ ))

#---------------------------------------------------------------

# Create the index file for the sflux master, if it exists.
FLUX_FILE="${COMIN_ATMOS_MASTER}/${PREFIX}sflux.${fhr3}.grib2"
if [[ -s "${FLUX_FILE}" ]]; then
    ${WGRIB2} -s "${FLUX_FILE}" > "${FLUX_FILE}.idx"
fi

# Section creating sflux grib2 interpolated products
# Create 1-degree sflux grib2 output
# move to COM and index it
if [[ "${FLXGF:-}" == "YES" ]]; then
    # Files needed by ${INTERP_ATMOS_SFLUXSH}
    input_file="${FLUX_FILE}"
    output_file_prefix="sflux_${fhr3}"
    grid_string="1p00"
    "${USHgfs}/interp_atmos_sflux.sh" "${input_file}" "${output_file_prefix}" "${grid_string}" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "FATAL ERROR: Unable to interpolate the surface flux grib2 files!"
    fi

    # Move to COM and index the product sflux file
    IFS=':' read -ra grids <<< "${grid_string}"
    for grid in "${grids[@]}"; do
        ${WGRIB2} -s "sflux_${fhr3}_${grid}" > "sflux_${fhr3}_${grid}.idx"
        prod_dir="COMOUT_ATMOS_GRIB_${grid}"
        cpfs "sflux_${fhr3}_${grid}" "${!prod_dir}/${PREFIX}flux.${grid}.${fhr3}.grib2"
        cpfs "sflux_${fhr3}_${grid}.idx" "${!prod_dir}/${PREFIX}flux.${grid}.${fhr3}.grib2.idx"
    done
fi

# Section creating 0.25 degree WGNE products for nset=1, and fhr <= FHMAX_WGNE
if [[ "${WGNE:-}" == "YES" ]]; then
    grp="a"
    if [[ "${FORECAST_HOUR}" -gt 0 && "${FORECAST_HOUR}" -le ${FHMAX_WGNE:-0} ]]; then
        # 598 is the message number for APCP in GFSv17 (it was 597 in GFSv16)
        ${WGRIB2} "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}pres_${grp}.0p25.${fhr3}.grib2" \
            -d "${APCP_MSG:-598}" \
            -grib "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}wgne.${fhr3}.grib2"
    fi
fi

#---------------------------------------------------------------

# Start sending DBN alerts
# Everything below this line is for sending files to DBN (SENDDBN=YES)
if [[ "${SENDDBN:-}" == "YES" ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25" "${job}" "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}pres_a.0p25.${fhr3}.grib2"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}pres_a.0p25.${fhr3}.grib2.idx"
    if [[ "${RUN}" == "gfs" ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25" "${job}" "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}pres_b.0p25.${fhr3}.grib2"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}pres_b.0p25.${fhr3}.grib2.idx"
        if [[ -s "${COMOUT_ATMOS_GRIB_0p50}/${PREFIX}pres_a.0p50.${fhr3}.grib2" ]]; then
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5" "${job}" "${COMOUT_ATMOS_GRIB_0p50}/${PREFIX}pres_a.0p50.${fhr3}.grib2"
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_0p50}/${PREFIX}pres_a.0p50.${fhr3}.grib2.idx"
        fi
        if [[ -s "${COMOUT_ATMOS_GRIB_0p50}/${PREFIX}pres_b.0p50.${fhr3}.grib2" ]]; then
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5" "${job}" "${COMOUT_ATMOS_GRIB_0p50}/${PREFIX}pres_b.0p50.${fhr3}.grib2"
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_0p50}/${PREFIX}pres_b.0p50.${fhr3}.grib2.idx"
        fi
        if [[ -s "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_a.1p00.${fhr3}.grib2" ]]; then
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_a.1p00.${fhr3}.grib2"
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_a.1p00.${fhr3}.grib2.idx"
        fi
        if [[ -s "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_b.1p00.${fhr3}.grib2" ]]; then
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_b.1p00.${fhr3}.grib2"
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_b.1p00.${fhr3}.grib2.idx"
        fi
        if [[ "${WGNE:-}" == "YES" ]] && [[ -s "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}wgne.${fhr3}.grib2" ]]; then
            "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_WGNE" "${job}" "${COMOUT_ATMOS_GRIB_0p25}/${PREFIX}wgne.${fhr3}.grib2"
        fi
    fi

    if [[ "${fhr3}" == "analysis" ]]; then

        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_MSC_sfcanl" "${job}" "${COMIN_ATMOS_ANALYSIS}/${PREFIX}analysis.sfc.a006.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SA" "${job}" "${COMIN_ATMOS_ANALYSIS}/${PREFIX}analysis.atm.a006.nc"

        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_a.1p00.${fhr3}.grib2"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_a.1p00.${fhr3}.grib2.idx"

    else # forecast hours f000, f003, f006, etc.

        case "${RUN}" in
            gdas)
                "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_a.1p00.${fhr3}.grib2"
                "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2_WIDX" "${job}" "${COMOUT_ATMOS_GRIB_1p00}/${PREFIX}pres_a.1p00.${fhr3}.grib2.idx"
                if ((FORECAST_HOUR % 3 == 0)); then
                    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF" "${job}" "${COMIN_ATMOS_HISTORY}/${PREFIX}atm.${fhr3}.nc"
                    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF" "${job}" "${COMIN_ATMOS_HISTORY}/${PREFIX}sfc.${fhr3}.nc"
                    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2" "${job}" "${COMIN_ATMOS_MASTER}/${PREFIX}sflux.f${fhr3}.grib2"
                    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COMIN_ATMOS_MASTER}/${PREFIX}sflux.f${fhr3}.grib2.idx"
                fi
                ;;
            gfs)
                "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF" "${job}" "${COMIN_ATMOS_HISTORY}/${PREFIX}atm.${fhr3}.nc"
                if [[ ${fhr} -gt 0 && ${fhr} -le 84 || ${fhr} -eq 120 ]]; then
                    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF" "${job}" "${COMIN_ATMOS_HISTORY}/${PREFIX}sfc.${fhr3}.nc"
                fi

                if [[ -s "${COMIN_ATMOS_MASTER}/${PREFIX}sflux.f${fhr3}.grib2" ]]; then
                    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2" "${job}" "${COMIN_ATMOS_MASTER}/${PREFIX}sflux.f${fhr3}.grib2"
                    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COMIN_ATMOS_MASTER}/${PREFIX}sflux.f${fhr3}.grib2.idx"
                fi
                ;;
            *)
                err_exit "Unsupported RUN value '${RUN}' for SENDDBN section"
                ;;
        esac
    fi
fi # end if SENDDBN=YES

exit 0
