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
export in_fname="'enkfgdas.sfci'"
export dir_coord_in="'${DATA}/'"
export dir_coord_out="'${DATA}/'"
export dir_mask_in="'${DATA}/'"
export dir_mask_out="'${DATA}/'"
export fname_mask_in="'NULL'"
export ires=${LONB_CASE_IN}
export jres=${LATB_CASE_IN}
export ireso=${CASE_OUT:1}
export jreso=${CASE_OUT:1}
export n_data="${NMEM_ENS}"

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

# input, fixed files
cpreq "${FIXorog}/${CASE_IN}/gaussian.${LONB_CASE_IN}.${LATB_CASE_IN}.nc" \
      "${DATA}/gaussian_scrip.nc"

# output, fixed files
cpreq "${FIXorog}/${CASE_OUT}/${CASE_OUT}_mosaic.nc" \
      "${DATA}/${CASE_OUT}_mosaic.nc"

for n in $(seq 1 "${ntiles}"); do
    cpreq "${FIXorog}/${CASE_OUT}/sfc/${CASE_OUT}.mx${OCNRES_OUT}.vegetation_type.tile${n}.nc" \
          "${DATA}/vegetation_type.tile${n}.nc"
    cpreq "${FIXorog}/${CASE_OUT}/${CASE_OUT}_grid.tile${n}.nc" \
          "${DATA}/${CASE_OUT}_grid.tile${n}.nc"
done

#export in_dir="("
export in_dir=""
for imem in $(seq 1 "${NMEM_ENS}"); do
    cmem=$(printf %03i "${imem}")
    memchar="mem${cmem}"

    # Create run directory for this member
    memdir="${DATA}/${memchar}"
    mkdir -p "${memdir}"

    if (( NMEM_ENS > 1 )); then
        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMOUT_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMIN_SOIL_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL
    fi

    # Append to input directory list
    if [ $imem -gt 1 ]; then
        in_dir+=", "
    fi
    in_dir+="\"${memdir}\""

    for FHR in "${soilinc_fhrs[@]}"; do
        cpreq "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}sfci00${FHR}.nc" \
               "${memdir}/enkfgdas.sfci00${FHR}.nc"
    done 

    if [[ "${DO_LAND_IAU}" = ".true." ]]; then 
        for FHI in "${landifhrs[@]}"; do
            cpreq "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}sfci00${FHI}.nc" \
                  "${memdir}/enkfgdas.sfci00${FHI}.nc"
        done
    fi
done

# Finish defining input/output directory list
#in_dir+=")"
export out_dir="${in_dir}"

#
# Regrid soil increments and save to COMOUT
#

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

for imem in $(seq 1 "${NMEM_ENS}"); do
    cmem=$(printf %03i "${imem}")
    memchar="mem${cmem}"
    memdir="${DATA}/${memchar}"

    for FHR in "${soilinc_fhrs[@]}"; do
        for n in $(seq 1 "${ntiles}"); do
            cpfs "${memdir}/sfci00${FHR}.tile${n}.nc"  "${COMOUT_ATMOS_ANALYSIS_MEM}/sfci00${FHR}.tile${n}.nc"
        done
    done

    if [[ "${DO_LAND_IAU}" = ".true." ]]; then
        for n in $(seq 1 "${ntiles}"); do
            cpfs "${memdir}/sfci.tile${n}.nc"  "${COMOUT_ATMOS_ANALYSIS_MEM}/sfc_inc.tile${n}.nc"
        done
    fi
done

exit 0
