#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh" "${FH}"

# Programs used
export WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}
CNVGRIB=${CNVGRIB:-${grib_util_ROOT}/bin/cnvgrib}
GRBINDEX=${GRBINDEX:-${wgrib2_ROOT}/bin/grbindex}

# Scripts used
GFSDWNSH=${GFSDWNSH:-"${HOMEgfs}/ush/fv3gfs_dwn_nems.sh"}

# variables used here and in $GFSDWNSH
PGBOUT2=${PGBOUT2:-"master.grib2"}  # grib2 file from UPP
FH=$(( ${FH:-0} ))  # Forecast hour to process
FHOUT_PGB=${FHOUT_PGB:-3}  # Output frequency of GFS PGB file at 1-degree and 0.5 degree
npe_dwn=${npe_dwn:-24}
downset=${downset:-1}
PREFIX=${PREFIX:-"${RUN:-gfs}.t${cyc}z."}
export PGBS=${PGBS:-"NO"}  # YES - generate 1 and 1/2-degree grib2 data
PGB1F=${PGB1F:-"NO"}  # YES - generate 1-degree grib1 data

# Files used
if (( FH == -1 )); then
  fhr3=anl
  export PGBS="YES"
  paramlista=${paramlist:-"${HOMEgfs}/parm/post/global_1x1_paramlist_g2.anl"}
elif (( FH == 0 )); then
  fhr3=000
  export PGBS="YES"
  paramlista=${paramlist:-"${HOMEgfs}/parm/post/global_1x1_paramlist_g2.f000"}
else
  fhr3=$(printf "%03d" "${FH}")
  if (( FH%FHOUT_PGB == 0 )); then
    export PGBS="YES"
  fi
  paramlista=${paramlist:-"${HOMEgfs}/parm/post/global_1x1_paramlist_g2"}
fi
paramlistb=${paramlistb:-"${HOMEgfs}/parm/post/global_master-catchup_parmlist_g2"}

# Get inventory from ${PGBOUT2} that matches patterns from ${paramlista}
# Extract this inventory from ${PGBOUT2} into a smaller tmpfile or tmpfileb based on paramlista or paramlistb
# shellcheck disable=SC2312
${WGRIB2} "${PGBOUT2}" | grep -F -f "${paramlista}" | ${WGRIB2} -i -grib "tmpfilea_${fhr3}" "${PGBOUT2}"
export err=$?; err_chk
# Do the same as above for ${paramlistb}
if (( downset = 2 )); then
  # shellcheck disable=SC2312
  ${WGRIB2} "${PGBOUT2}" | grep -F -f "${paramlistb}" | ${WGRIB2} -i -grib "tmpfileb_${fhr3}" "${PGBOUT2}"
  export err=$?; err_chk
fi

# Determine grids once # Cannot export arrays in bash, so need to export PGBS
grids=("0p25")
if [[ "${PGBS}" = "YES" ]]; then
  grids+=("0p50")
  grids+=("1p00")
fi

#-----------------------------------------------------
nproc=${nproc:-${npe_dwn}}

#..............................................
for (( nset=1 ; nset <= downset ; nset++ )); do

  echo "Begin processing nset = ${nset}"

  # Each set represents a group of files
  if (( nset == 1 )); then
    grp="a"
  elif (( nset == 2 )); then
    grp="b"
  fi

  # process Grib files to run downstream jobs using MPMD
  tmpfile="tmpfile${grp}_${fhr3}"

  # shellcheck disable=SC2312
  ncount=$(${WGRIB2} "${tmpfile}" | wc -l)
  if (( nproc > ncount )); then
    echo "FATAL ERROR: Total number of records in ${tmpfile} is not right"  # No, the no. of records < no. of processors
    export err=8
    err_chk
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
    echo "${GFSDWNSH} ${input_file} ${output_file_prefix}" >> "${DATA}/poescript"

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
    ${NCP} "pgb2${grp}file_${fhr3}_${grid}" "${COM_ATMOS_GRIB}/${grid}${PREFIX}pgrb2${grp}.${grid}.${fhr3}"
    ${WGRIB2} -s "pgb2${grp}file_${fhr3}_${grid}" > "${COM_ATMOS_GRIB}/${grid}/${PREFIX}pgrb2${grp}.${grid}.${fhr3}.idx"
  done

  # Create supplemental 1-degree grib1 output TODO: who needs 1-degree grib1 product?
  # move to COM and index it
  if (( nset == 1 )); then
    if [[ "${PGBS}" = "YES" ]]; then
      if [[ "${PGB1F}" = "YES" ]]; then
        ${CNVGRIB} -g21 "pgb2${grp}file_${fhr3}_1p00" "pgb${grp}file_${fhr3}_1p00"
        export err=$?; err_chk
        ${NCP} "pgb${grp}file_${fhr3}_1p00" "${COM_ATMOS_GRIB}/1p00/${PREFIX}pgrb${grp}.1p00.${fhr3}"
        ${GRBINDEX} "${COM_ATMOS_GRIB}/1p00/${PREFIX}pgrb${grp}.1p00.${fhr3}" "${COM_ATMOS_GRIB}/1p00/${PREFIX}pgrb${grp}.1p00.${fhr3}.idx"
      fi
    fi
  fi

  echo "Finished processing nset = ${nset}"


done  # for (( nset=1 ; nset <= downset ; nset++ ))

exit 0
