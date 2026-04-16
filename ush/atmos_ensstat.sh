#! /usr/bin/env bash

#===============================================================================
#
#   FILE: atmos_ensstat.sh
#
#   DESCRIPTION: This script processes ensemble forecast output for a specific
#                grid and forecast hour. It collects atmospheric GRIB2 files
#                for all ensemble members, dynamically generates a namelist,
#                and executes `ensstat.x` to compute the ensemble mean and
#                spread. Finally, it indexes the outputs using wgrib2, moves
#                them to the designated COM directory, and issues DBN alerts.
#    ARGUMENTS:
#       $1 - grid      : The grid resolution/identifier (e.g., 0p25, 1p00).
#       $2 - fhr3      : The 3-digit forecast hour (e.g., 012, 024).
#       $3 - grid_type : (Optional) Grid type identifier (defaults to empty).
#
#       OUTPUTS:
#       Produces mean and spread GRIB2 files and their corresponding .idx
#       inventory files in the configured COMOUT directory.

grid=${1}
fhr3=${2}
grid_type=${3:-''}

mkdir "${grid}${grid_type}"
cd "${grid}${grid_type}" || exit 2

# Collect input grib files
input_files=()
for ((mem_num = 0; mem_num <= "${NMEM_ENS:-0}"; mem_num++)); do
    mem=$(printf "%03d" "${mem_num}")
    COMIN_ATMOS_GRIB="${ROTDIR}/${RUN}.${PDY}/${cyc}/mem${mem}/products/atmos/grib2/${grid}"
    memfile_in="${COMIN_ATMOS_GRIB}/${RUN}.t${cyc}z.pres_a${grid_type}.${grid}.f${fhr3}.grib2"

    if [[ -r "${memfile_in}.idx" ]]; then
        ${NLN} "${memfile_in}" "mem${mem}"
        input_files+=("mem${mem}")
    else
        echo "FATAL ERROR: ${memfile_in} does not exist"
        exit 10
    fi
done

num_found=${#input_files[@]}
if ((num_found != NMEM_ENS + 1)); then
    echo "FATAL ERROR: Only ${num_found} grib files found out of $((NMEM_ENS + 1)) expected members."
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
    for ((filenum = 1; filenum <= num_found; filenum++)); do
        echo "    cfipg(${filenum})=\"${input_files[$((filenum - 1))]}\","
        echo "    iskip(${filenum})=0,"
    done
)
/
EOF

cat input.nml

# Run ensstat
"${EXECglobal}/ensstat.x" < input.nml

export err=$?
if [[ "${err}" -ne 0 ]]; then
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
    if [[ "${err}" -ne 0 ]]; then
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
