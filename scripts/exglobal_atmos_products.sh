#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

# Programs used
export WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}

# Scripts used
INTERP_ATMOS_MASTERSH=${INTERP_ATMOS_MASTERSH:-"${USHgfs}/interp_atmos_master.sh"}
INTERP_ATMOS_SFLUXSH=${INTERP_ATMOS_SFLUXSH:-"${USHgfs}/interp_atmos_sflux.sh"}

# Variables used in this job
downset=${downset:-1}  # No. of groups of pressure grib2 products to create
ntasks_atmos_products=${ntasks_atmos_products:-8}  # no. of processors available to process each group

# WGNE related options
WGNE=${WGNE:-NO}  # Create WGNE products
FHMAX_WGNE=${FHMAX_WGNE:-0}  # WGNE products are created for first FHMAX_WGNE forecast hours (except 0)

cd "${DATA}" || exit 1

# Set paramlist files based on FORECAST_HOUR (-1, 0, 3, 6, etc.)
# Determine if supplemental products (PGBS) (1-degree and 1/2-degree) should be generated
if (( FORECAST_HOUR <= 0 )); then
  if (( FORECAST_HOUR < 0 )); then
    fhr3="anl"
    paramlista="${paramlista_anl}"
    FLXGF="NO"
  elif (( FORECAST_HOUR == 0 )); then
    fhr3=$(printf "f%03d" "${FORECAST_HOUR}")
    paramlista="${paramlista_f000}"
  fi
  PGBS="YES"
else
  fhr3=$(printf "f%03d" "${FORECAST_HOUR}")
  if (( FORECAST_HOUR%FHOUT_PGBS == 0 )); then
    PGBS="YES"
  fi
fi

#-----------------------------------------------------
# Section creating pressure grib2 interpolated products

# Files needed by ${INTERP_ATMOS_MASTERSH}
MASTER_FILE="${COM_ATMOS_MASTER}/${PREFIX}master.grb2${fhr3}"

# Get inventory from ${MASTER_FILE} that matches patterns from ${paramlista}
# Extract this inventory from ${MASTER_FILE} into a smaller tmpfile or tmpfileb based on paramlista or paramlistb
# shellcheck disable=SC2312
${WGRIB2} "${MASTER_FILE}" | grep -F -f "${paramlista}" | ${WGRIB2} -i -grib "tmpfile_${fhr3}" "${MASTER_FILE}"
export err=$?; err_chk
# Do the same as above for ${paramlistb}
if (( downset == 2 )); then
  # shellcheck disable=SC2312
  ${WGRIB2} "${MASTER_FILE}" | grep -F -f "${paramlistb}" | ${WGRIB2} -i -grib "tmpfileb_${fhr3}" "${MASTER_FILE}"
  export err=$?; err_chk
fi

# Determine grids once and save them as a string and an array for processing
grid_string="0p25"
if [[ "${PGBS:-}" == "YES" ]]; then
  grid_string="${grid_string}:0p50:1p00"
else
  echo "Supplemental product generation is disable for fhr = ${fhr3}"
  PGBS="NO"  # Can't generate supplemental products if PGBS is not YES
fi
# Also transform the ${grid_string} into an array for processing
IFS=':' read -ra grids <<< "${grid_string}"

for (( nset=1 ; nset <= downset ; nset++ )); do

  echo "Begin processing nset = ${nset}"

  # Number of processors available to process $nset
  nproc=${ntasks}

  # Each set represents a group of files
  if (( nset == 1 )); then
    grp=""  # TODO: this should be "a" when we eventually rename the pressure grib2 files per EE2 convention
  elif (( nset == 2 )); then
    grp="b"
  fi

  # process grib2 chunkfiles to interpolate using MPMD
  tmpfile="tmpfile${grp}_${fhr3}"

  # shellcheck disable=SC2312
  ncount=$(${WGRIB2} "${tmpfile}" | wc -l)
  if (( nproc > ncount )); then
    echo "WARNING: Total no. of available processors '${nproc}' exceeds no. of records '${ncount}' in ${tmpfile}"
    echo "Reduce nproc to ${ncount} (or less) to not waste resources"
  fi
  inv=$(( ncount / nproc ))
  rm -f "${DATA}/poescript"

  last=0
  for (( iproc = 1 ; iproc <= nproc ; iproc++ )); do
    first=$((last + 1))
    last=$((last + inv))
    if (( last > ncount )); then (( last = ncount )); fi

    # if final record of is u-component, add next record v-component
    # if final record is land, add next record icec
    # grep returns 1 if no match is found, so temporarily turn off exit on non-zero rc
    set +e
    # shellcheck disable=SC2312
    ${WGRIB2} -d "${last}" "${tmpfile}" | grep -E -i "ugrd|ustm|uflx|u-gwd|land|maxuw"
    rc=$?
    set_strict
    if (( rc == 0 )); then  # Matched the grep
      last=$(( last + 1 ))
    fi
    if (( iproc == nproc )); then
      last=${ncount}
    fi

    # Break tmpfile into processor specific chunks in preparation for MPMD
    ${WGRIB2} "${tmpfile}" -for "${first}":"${last}" -grib "${tmpfile}_${iproc}"
    export err=$?; err_chk
    input_file="${tmpfile}_${iproc}"
    output_file_prefix="pgb2${grp}file_${fhr3}_${iproc}"
    echo "${INTERP_ATMOS_MASTERSH} ${input_file} ${output_file_prefix} ${grid_string}" >> "${DATA}/poescript"

    # if at final record and have not reached the final processor then write echo's to
    # poescript for remaining processors
    if (( last == ncount )); then
      for (( pproc = iproc+1 ; pproc < nproc ; pproc++ )); do
        echo "/bin/echo ${pproc}" >> "${DATA}/poescript"
      done
      break
    fi
  done  # for (( iproc = 1 ; iproc <= nproc ; iproc++ )); do

  # Run with MPMD or serial
  if [[ "${USE_CFP:-}" = "YES" ]]; then
    OMP_NUM_THREADS=1 "${USHgfs}/run_mpmd.sh" "${DATA}/poescript"
    export err=$?
  else
    chmod 755 "${DATA}/poescript"
    bash +x "${DATA}/poescript" > mpmd.out 2>&1
    export err=$?
  fi
  err_chk

  # We are in a loop over downset, save output from mpmd into nset specific output
  cat mpmd.out  # so we capture output into the main logfile
  mv mpmd.out "mpmd_${nset}.out"

  # Concatenate grib files from each processor into a single one
  # and clean-up as you go
  echo "Concatenating processor-specific grib2 files into a single product file"
  for (( iproc = 1 ; iproc <= nproc ; iproc++ )); do
    for grid in "${grids[@]}"; do
      cat "pgb2${grp}file_${fhr3}_${iproc}_${grid}" >> "pgb2${grp}file_${fhr3}_${grid}"
      rm  "pgb2${grp}file_${fhr3}_${iproc}_${grid}"
    done
    # There is no further use of the processor specific tmpfile; delete it
    rm "${tmpfile}_${iproc}"
  done

  # Move to COM and index the product grib files
  for grid in "${grids[@]}"; do
    prod_dir="COM_ATMOS_GRIB_${grid}"
    ${NCP} "pgb2${grp}file_${fhr3}_${grid}" "${!prod_dir}/${PREFIX}pgrb2${grp}.${grid}.${fhr3}"
    ${WGRIB2} -s "pgb2${grp}file_${fhr3}_${grid}" > "${!prod_dir}/${PREFIX}pgrb2${grp}.${grid}.${fhr3}.idx"
  done

  echo "Finished processing nset = ${nset}"

done  # for (( nset=1 ; nset <= downset ; nset++ ))

#---------------------------------------------------------------

# Create the index file for the sflux master, if it exists.
FLUX_FILE="${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2"
if [[ -s "${FLUX_FILE}" ]]; then
  ${WGRIB2} -s "${FLUX_FILE}" > "${FLUX_FILE}.idx"
fi

# Section creating slfux grib2 interpolated products
# Create 1-degree sflux grib2 output
# move to COM and index it
if [[ "${FLXGF:-}" == "YES" ]]; then

  # Files needed by ${INTERP_ATMOS_SFLUXSH}
  input_file="${FLUX_FILE}"
  output_file_prefix="sflux_${fhr3}"
  grid_string="1p00"
  "${INTERP_ATMOS_SFLUXSH}" "${input_file}" "${output_file_prefix}" "${grid_string}"
  export err=$?; err_chk

  # Move to COM and index the product sflux file
  IFS=':' read -ra grids <<< "${grid_string}"
  for grid in "${grids[@]}"; do
    prod_dir="COM_ATMOS_GRIB_${grid}"
    ${NCP} "sflux_${fhr3}_${grid}" "${!prod_dir}/${PREFIX}flux.${grid}.${fhr3}"
    ${WGRIB2} -s "sflux_${fhr3}_${grid}" > "${!prod_dir}/${PREFIX}flux.${grid}.${fhr3}.idx"
  done
fi

# Section creating 0.25 degree WGNE products for nset=1, and fhr <= FHMAX_WGNE
if [[ "${WGNE:-}" == "YES" ]]; then
  grp=""  # TODO: this should be "a" when we eventually rename the pressure grib2 files per EE2 convention
  if (( FORECAST_HOUR > 0 & FORECAST_HOUR <= FHMAX_WGNE )); then
    # TODO: 597 is the message number for APCP in GFSv16.  GFSv17 may change this as more messages are added. This can be controlled via config.atmos_products
    ${WGRIB2} "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2${grp}.0p25.${fhr3}" -d "${APCP_MSG:-597}" -grib "${COM_ATMOS_GRIB_0p25}/${PREFIX}wgne.${fhr3}"
  fi
fi

#---------------------------------------------------------------

# Start sending DBN alerts
# Everything below this line is for sending files to DBN (SENDDBN=YES)
if [[ "${SENDDBN:-}" == "YES" ]]; then
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25"       "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.${fhr3}"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.${fhr3}.idx"
  if [[ "${RUN}" == "gfs" ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25"      "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25_WIDX" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.${fhr3}.idx"
    if [[ -s "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.${fhr3}" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5"       "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.${fhr3}.idx"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5"      "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5_WIDX" "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.${fhr3}.idx"
    fi
    if [[ -s "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0"       "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0_WIDX"  "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}.idx"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0"      "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.${fhr3}.idx"
    fi
    if [[ "${WGNE:-}" == "YES" ]] && [[ -s "${COM_ATMOS_GRIB_0p25}/${PREFIX}wgne.${fhr3}" ]] ; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_WGNE" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}wgne.${fhr3}"
    fi
  fi

  if [[ "${fhr3}" == "anl" ]]; then

    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_MSC_sfcanl" "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}sfc${fhr3}.nc"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SA"         "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}atm${fhr3}.nc"

    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2"      "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}.idx"

  else  # forecast hours f000, f003, f006, etc.

    if [[ "${RUN}" == "gdas" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2"        "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2_WIDX"   "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}.idx"
      if (( FORECAST_HOUR % 3 == 0 )); then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF"           "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atm${fhr3}.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF"           "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfc${fhr3}.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2"      "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2.idx"
      fi
    elif [[ "${RUN}" == "gfs" ]]; then

      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atm${fhr3}.nc"
      if (( fhr > 0 && fhr <= 84 )) || (( fhr == 120 )); then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfc${fhr3}.nc"
      fi

      if [[ -s "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2" ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2"      "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2.idx"
      fi
    fi  # end if RUN=gfs

  fi  # end if fhr3=anl

fi  # end if SENDDBN=YES

exit 0
