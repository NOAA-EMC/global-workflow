#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

# Programs used
export WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}
CNVGRIB=${CNVGRIB:-${grib_util_ROOT}/bin/cnvgrib}
GRBINDEX=${GRBINDEX:-${wgrib2_ROOT}/bin/grbindex}

# Scripts used
INTERP_ATMOS_MASTERSH=${INTERP_ATMOS_MASTERSH:-"${HOMEgfs}/ush/interp_atmos_master.sh"}

# variables used here and in $GFSDWNSH
MASTER_FILE=${MASTER_FILE:-"master.grib2"}  # grib2 file from UPP
downset=${downset:-1}
npe_atmos_products=${npe_atmos_products:-8}  # no. of processors available to process each downset
PGBS=${PGBS:-"NO"}  # YES - generate 1 and 1/2-degree grib2 data
PGB1F=${PGB1F:-"NO"}  # YES - generate 1-degree grib1 data
PREFIX=${PREFIX:-"${RUN:-gfs}.t${cyc}z."}

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
if [[ "${PGBS}" == "YES" ]]; then
  grid_string="${grid_string}:0p50:1p00"
fi
# Also transform the ${grid_string} into an array for processing
IFS=':' read -ra grids <<< "${grid_string}"

#-----------------------------------------------------
for (( nset=1 ; nset <= downset ; nset++ )); do

  echo "Begin processing nset = ${nset}"

  # Number of processors available to process $nset
  nproc=${npe_atmos_products}

  # Each set represents a group of files
  if (( nset == 1 )); then
    grp=""  # TODO: this should be "a" when we eventually rename the pressure grib2 files per EE2 convention
  elif (( nset == 2 )); then
    grp="b"
  fi

  # process Grib files to run downstream jobs using MPMD
  tmpfile="tmpfile${grp}_${fhr3}"

  # shellcheck disable=SC2312
  ncount=$(${WGRIB2} "${tmpfile}" | wc -l)
  if (( nproc > ncount )); then
    echo "WARNING: Total no. of available processors '${nproc}' exceeds no. of records '${ncount}' in ${tmpfile}"
    echo "Reset nproc=${ncount}"
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
    ${WGRIB2} -d "${last}" "${tmpfile}" | grep -E -i "ugrd|ustm|uflx|u-gwd|land"
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
    "${HOMEgfs}/ush/run_mpmd.sh" "${DATA}/poescript"
    export err=$?
  else
    chmod 755 "${DATA}/poescript"
    bash +x "${DATA}/poescript" 2>&1 mpmd.out
    export err=$?
  fi
  err_chk

  # We are in a loop over downset, save output from mpmd into nset specific output
  cat mpmd.out  # so we capture output into the main logfile
  mv mpmd.out "mpmd_${nset}.out"

  # Concatenate grib files from each processor into a single one
  # and clean-up as you go
  echo "Concatenating processor specific grib2 files into a single product"
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

  # Create supplemental 1-degree grib1 output TODO: who needs 1-degree grib1 product?
  # move to COM and index it
  if (( nset == 1 )); then
    if [[ "${PGBS}" == "YES" ]]; then
      if [[ "${PGB1F}" == "YES" ]]; then
        ${CNVGRIB} -g21 "pgb2${grp}file_${fhr3}_1p00" "pgb${grp}file_${fhr3}_1p00"
        export err=$?; err_chk
        ${NCP} "pgb${grp}file_${fhr3}_1p00" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb${grp}.1p00.${fhr3}"
        ${GRBINDEX} "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb${grp}.1p00.${fhr3}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb${grp}.1p00.${fhr3}.idx"
      fi
    fi
  fi

  echo "Finished processing nset = ${nset}"


done  # for (( nset=1 ; nset <= downset ; nset++ ))

exit 0
