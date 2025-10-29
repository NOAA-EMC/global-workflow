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
export in_fname="'enkfgdas.sfci'"
export out_fname="'sfci'"
export dir_mask_in="'./'"
export fname_mask_in="'NULL'"
export ires=${LONB_CASE_IN}
export jres=${LATB_CASE_IN}
export ireso=${CASE_OUT:1}
export jreso=${CASE_OUT:1}

regrid_nml_tmpl="${PARMgfs}/regrid_sfc/regrid.nml_tmpl" 

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

if (( LFHR >= 0 )); then
    soilinc_fhrs=("${LFHR}")
else # construct restart times for deterministic member
    soilinc_fhrs=("${assim_freq}") # increment file at middle of window
    if [[ "${DOIAU:-}" == "YES" ]]; then  # Update surface restarts at beginning of window
        half_window=$(( assim_freq / 2 ))
        soilinc_fhrs+=("${half_window}")
    fi
fi

for imem in $(seq 1 "${NMEM_REGRID}"); do
    if (( NMEM_REGRID > 1 )); then
        cmem=$(printf %03i "${imem}")
        memchar="mem${cmem}"

        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMOUT_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

        MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMIN_SOIL_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL
    fi

    for FHR in "${soilinc_fhrs[@]}"; do

        export add_time_dim=".false."
        export time_list="${FHR}"

        rm -f "regrid.nml"
        atparse < "${regrid_nml_tmpl}" >> "regrid.nml"

        cpreq "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}increment.sfc.i00${FHR}.nc" \
               "${DATA}/enkfgdas.sfci00${FHR}.nc"

        ${APRUN_REGRID} "${REGRID_EXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"

        for n in $(seq 1 "${ntiles}"); do
            cpfs "${DATA}/sfci.tile${n}.nc"  "${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.i00${FHR}.tile${n}.nc"
        done
    done 

    if [[ "${DO_LAND_IAU}" = ".true." ]]; then 

        export add_time_dim=".true."
        export time_list="${IAUFHRS}"

        rm -f "regrid.nml"
        atparse < "${regrid_nml_tmpl}" >> "regrid.nml"

        for FHI in "${landifhrs[@]}"; do
            cpreq "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}increment.sfc.i00${FHI}.nc" \
                  "${DATA}/enkfgdas.sfci00${FHI}.nc"
        done
        
        export pgm="${REGRID_EXEC}"
	      ${APRUN_REGRID} "${REGRID_EXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"
	      export err=$?
	      if [[ ${err} -ne 0 ]]; then
	          err_exit "${pgm} failed, ABORT!"
	      fi

        for n in $(seq 1 "${ntiles}"); do
            cpfs "${DATA}/sfci.tile${n}.nc"  "${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.i006.tile${n}.nc"
        done
	    
    fi

done

exit 0

