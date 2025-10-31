#! /usr/bin/env bash

source "${HOMEgfs}/ush/atparse.bash"

#-------------------------------------------------------------------------------------------------
# Script to regrid surface increment from GSI grid
# to fv3 tiles.
# Clara Draper, Dec 2024
#-------------------------------------------------------------------------------------------------

export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export REDOUT=${REDOUT:-'1>'}
export REDERR=${REDERR:-'2>'}

export PGM=${REGRID_EXEC}
export pgm=${PGM}

# Use CFP for ensemble job
if [[ "${RUN}" == *"enkf"* ]]; then
    export USE_CFP=YES
else
    export USE_CFP=NO
fi

NMEM_REGRID=${NMEM_REGRID:-1}
CASE_IN=${CASE_IN:-${CASE_ENS}}
LFHR=${LFHR:-6}

# get resolutions
LONB_CASE_IN=$((4*${CASE_IN:1}))
LATB_CASE_IN=$((2*${CASE_IN:1}))

ntiles=6

APREFIX_ENS="enkfgdas.t${cyc}z."

LSOIL_INCR=${LSOIL_INCR:-2}

export n_vars=$(( LSOIL_INCR*2 ))

soil_incr_vars=""
for vi in $( seq 1 "${LSOIL_INCR}" ); do
    soil_incr_vars=${soil_incr_vars}'"soilt'${vi}'_inc"',
done
for vi in $( seq 1 "${LSOIL_INCR}" ); do
    soil_incr_vars=${soil_incr_vars}'"slc'${vi}'_inc"',
done

if [[ "${DO_LAND_IAU}" = ".true." ]]; then
    IFS=',' read -ra landifhrs <<< "${IAUFHRS}"
fi
export in_fname="'sfci'"
export out_fname="'sfci'"
export in_dir=""
export dir_mask_in="'./'"
export fname_mask_in="'NULL'"
export ires=${LONB_CASE_IN}
export jres=${LATB_CASE_IN}
export ireso=${CASE_OUT:1}
export jreso=${CASE_OUT:1}

regrid_nml_tmpl="${PARMgfs}/regrid_sfc/regrid.nml_tmpl" 

if (( LFHR >= 0 )); then
    soilinc_fhrs=("${LFHR}")
else # construct restart times for deterministic member
    soilinc_fhrs=("${assim_freq}") # increment file at middle of window
    if [[ "${DOIAU:-}" == "YES" ]]; then  # Update surface restarts at beginning of window
        half_window=$(( assim_freq / 2 ))
        soilinc_fhrs+=("${half_window}")
    fi
fi

#
# Stage input files
#

# Create master MPMD command file
rm -f cmdfile
touch cmdfile
chmod 755 cmdfile

# Create MDMP command file for fixed files
rm -f cmdfile.0
touch cmdfile.0
chmod 755 cmdfile.0
echo "#!/bin/bash" > cmdfile.0

# input, fixed files
echo "cpreq ${FIXorog}/${CASE_IN}/gaussian.${LONB_CASE_IN}.${LATB_CASE_IN}.nc \
            ${DATA}/gaussian_scrip.nc" >> cmdfile.0

# output, fixed files
echo "cpreq ${FIXorog}/${CASE_OUT}/${CASE_OUT}_mosaic.nc \
            ${DATA}/${CASE_OUT}_mosaic.nc" >> cmdfile.0

for n in $(seq 1 "${ntiles}"); do
    echo "cpreq ${FIXorog}/${CASE_OUT}/sfc/${CASE_OUT}.mx${OCNRES_OUT}.vegetation_type.tile${n}.nc \
                ${DATA}/vegetation_type.tile${n}.nc" >> cmdfile.0
    echo "cpreq ${FIXorog}/${CASE_OUT}/${CASE_OUT}_grid.tile${n}.nc \
                ${DATA}/${CASE_OUT}_grid.tile${n}.nc" >> cmdfile.0
done

# Append fixed files command file to master command file
echo "${DATA}/cmdfile.0" >> cmdfile

for imem in $(seq 1 "${NMEM_REGRID}"); do
    cmem=$(printf %03i "${imem}")
    memchar="mem${cmem}"

    if (( NMEM_REGRID > 1 )); then
        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMOUT_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMIN_SOIL_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

        memdir="${DATA}/${memchar}"
        mkdir -p "${memdir}"

        if [[ "${imem}" -gt 1 ]]; then
            in_dir+=", "
        fi
        in_dir+="\"./${memchar}/\""        
    else
        memdir="${DATA}"

        in_dir="'./'"
    fi

    # Create MPMD command file for this member
    rm -f "cmdfile.${imem}"
    touch "cmdfile.${imem}"
    chmod 755 "cmdfile.${imem}"
    echo "#!/bin/bash" > "cmdfile.${imem}"

    for FHR in "${soilinc_fhrs[@]}"; do
        echo "cpreq ${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}increment.sfc.i00${FHR}.nc \
                    ${memdir}/sfci00${FHR}.nc" >> "cmdfile.${imem}"
    done 

    if [[ "${DO_LAND_IAU}" = ".true." ]]; then 
        for FHI in "${landifhrs[@]}"; do
            echo "cpreq ${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}increment.sfc.i00${FHI}.nc \
                        ${memdir}/sfci00${FHI}.nc" >> "cmdfile.${imem}"
        done
    fi

    # Append this member's command file to master command file
    echo "${DATA}/cmdfile.${imem}" >> cmdfile
done

# Run MPMD to stage input files
"${USHgfs}/run_mpmd.sh" "cmdfile" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "run_mpmd.sh failed!"
fi

# Finish defining input/output directory list
export out_dir="${in_dir}"

#
# Regrid soil increments and save to COMOUT
#

if [[ "${DO_LAND_IAU}" = ".false." || "${RUN}" == "gdas" || "${RUN}" == "gfs" ]]; then
    for FHR in "${soilinc_fhrs[@]}"; do
        export add_time_dim=".false."
        export time_list="${FHR}"
        export out_fname="'sfci00${FHR}'"

        rm -f "regrid.nml"
        atparse < "${regrid_nml_tmpl}" >> "regrid.nml"

        ${APRUN_REGRID} "${REGRID_EXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"
    	export err=$?
	if [[ ${err} -ne 0 ]]; then
	    err_exit "${REGRID_EXEC} failed, ABORT!"
	fi
    done
fi

if [[ "${DO_LAND_IAU}" = ".true." ]]; then
    export add_time_dim=".true."
    export time_list="${IAUFHRS}"
    export out_fname="'sfci'"

    rm -f "regrid.nml"
    atparse < "${regrid_nml_tmpl}" >> "regrid.nml"

    export pgm="${REGRID_EXEC}"
	${APRUN_REGRID} "${REGRID_EXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"
	export err=$?
	if [[ ${err} -ne 0 ]]; then
	    err_exit "${pgm} failed, ABORT!"
	fi
fi

#
# Save regridded files to COMOUT
#

# Create master MPMD command file
rm -f cmdfile
touch cmdfile
chmod 755 cmdfile

for imem in $(seq 1 "${NMEM_REGRID}"); do
    cmem=$(printf %03i "${imem}")
    memchar="mem${cmem}"

    if (( NMEM_REGRID > 1 )); then
        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMOUT_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMIN_SOIL_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

        memdir="${DATA}/${memchar}"
    else
        memdir="${DATA}"
    fi

    # Create MPMD command file for this member
    rm -f "cmdfile.${imem}"
    touch "cmdfile.${imem}"
    chmod 755 "cmdfile.${imem}"
    echo "#!/bin/bash" > "cmdfile.${imem}"

    if [[ "${DO_LAND_IAU}" = ".false." || "${RUN}" == "gdas" || "${RUN}" == "gfs" ]]; then
        for FHR in "${soilinc_fhrs[@]}"; do
            for n in $(seq 1 "${ntiles}"); do
                echo "cpfs ${memdir}/sfci00${FHR}.mem${imem}.tile${n}.nc \
                      ${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.i00${FHR}.tile${n}.nc" >> "cmdfile.${imem}"
            done
        done
    fi

    if [[ "${DO_LAND_IAU}" = ".true." ]]; then
        for n in $(seq 1 "${ntiles}"); do
            echo "cpfs ${memdir}/sfci.mem${imem}.tile${n}.nc \
                  ${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.i006.tile${n}.nc" >> "cmdfile.${imem}"
        done
    fi

    # Append this member's command file to master command file
    echo "${DATA}/cmdfile.${imem}" >> cmdfile
done

# Run MPMD to save output files
"${USHgfs}/run_mpmd.sh" "cmdfile" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "run_mpmd.sh failed!"
fi

exit 0
