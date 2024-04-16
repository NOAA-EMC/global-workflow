#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

grid=${1}
fhr3=${2}
grid_type=${3:-''}

mkdir "${grid}${grid_type}"
cd "${grid}${grid_type}" || exit 2

# Collect input grib files
input_files=()
for ((mem_num = 0; mem_num <= "${NMEM_ENS:-0}"; mem_num++)); do
    mem=$(printf "%03d" "${mem_num}")
    MEMDIR="mem${mem}" GRID="${grid}" YMD="${PDY}" HH="${cyc}" declare_from_tmpl COMIN_ATMOS_GRIB:COM_ATMOS_GRIB_GRID_TMPL
    memfile_in="${COMIN_ATMOS_GRIB}/${RUN}.t${cyc}z.pgrb2${grid_type}.${grid}.f${fhr3}"

    if [[ -r "${memfile_in}.idx" ]]; then
        ${NLN} "${memfile_in}" "mem${mem}"
        input_files+=("mem${mem}")
    else
        echo "FATAL ERROR: ${memfile_in} does not exist"
        exit 10
    fi
done

num_found=${#input_files[@]}
if (( num_found != NMEM_ENS + 1 )); then
    echo "FATAL ERROR: Only ${num_found} grib files found out of $(( NMEM_ENS + 1 )) expected members."
    exit 10
fi

# Create namelist for ensstat
mean_out="${RUN}.t${cyc}z.mean.pres_${grid_type}.${grid}.f${fhr3}.grib2"
spr_out="${RUN}.t${cyc}z.spread.pres_${grid_type}.${grid}.f${fhr3}.grib2"

cat << EOF > input.nml
&namdim
    lfdim=${lfm:-''}
/

&namens
    nfiles=${num_found}
    nenspost=0
    navg_min=${NMEM_ENS}

    cfopg1="${mean_out}"
    cfopg2="${spr_out}"

$(
    for (( filenum = 1; filenum <= num_found; filenum++ )); do
        echo "    cfipg(${filenum})=\"${input_files[$((filenum-1))]}\","
        echo "    iskip(${filenum})=0,"
    done
)
/
EOF

cat input.nml

# Run ensstat
"${EXECgfs}/ensstat.x" < input.nml

export err=$?
if (( err != 0 )) ; then
    echo "FATAL ERROR: ensstat returned error code ${err}"
    exit "${err}"
fi

# Send data to com and send DBN alerts
comout_var_name="COMOUT_ATMOS_GRIB_${grid}"
comout_path="${!comout_var_name}"

for outfile in ${mean_out} ${spr_out}; do
    if [[ ! -s ${outfile} ]]; then
        echo "FATAL ERROR: Failed to create ${outfile}"
        exit 20
    fi

    ${WGRIB2} -s "${outfile}" > "${outfile}.idx"
    err=$?
    if (( err != 0 )); then
        echo "FATAL ERROR: Failed to create inventory file, wgrib2 returned ${err}"
        exit "${err}"
    fi

    cpfs "${outfile}" "${comout_path}/${outfile}"
    cpfs "${outfile}.idx" "${comout_path}/${outfile}.idx"

    if [[ ${SENDDBN} == "YES" ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2${grid_type}_${grid}" "${job}" \
            "${comout_path}/${outfile}"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2${grid_type}_${grid}" "${job}" \
            "${comout_path}/${outfile}.idx"
    fi

done

