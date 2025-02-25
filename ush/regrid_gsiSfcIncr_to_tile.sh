#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

#-------------------------------------------------------------------------------------------------
# Script to regrid surface increment from GSI grid 
# to fv3 tiles. 
# Clara Draper, Dec 2024
#-------------------------------------------------------------------------------------------------

# temporary files on hera, until g-w issue 3392 is resolved.
TMP_FIX_FILES=/scratch2/BMC/gsienkf/Clara.Draper/gw_new_fix_files/


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

n_vars=$(( LSOIL_INCR*2 ))

soil_incr_vars=""
for vi in $( seq 1 "${LSOIL_INCR}" ); do
    soil_incr_vars=${soil_incr_vars}'"soilt'${vi}'_inc"',
done
for vi in $( seq 1 "${LSOIL_INCR}" ); do
    soil_incr_vars=${soil_incr_vars}'"slc'${vi}'_inc"',
done

cat << EOF > regrid.nml
 &config
  n_vars=${n_vars},
  variable_list=${soil_incr_vars}
  missing_value=0.,
 /
 &input
  gridtype="gau_inc",
  ires=${LONB_CASE_IN},
  jres=${LATB_CASE_IN},
  fname="enkfgdas.sfci.nc",
  dir="./",
  fname_coord="gaussian_scrip.nc",
  dir_coord="./"
/

 &output
  gridtype="fv3_rst",
  ires=${CASE_OUT:1},
  jres=${CASE_OUT:1},
  fname="sfci",
  dir="./",
  fname_mask="vegetation_type" 
  dir_mask="./"
  dir_coord="./",
 /
EOF

# input, fixed files
${NCP} "${TMP_FIX_FILES}/gaussian.${LONB_CASE_IN}.${LATB_CASE_IN}.nc" \
        "${DATA}/gaussian_scrip.nc"

# output, fixed files
${NCP} "${FIXorog}/${CASE_OUT}/${CASE_OUT}_mosaic.nc" \
        "${DATA}/${CASE_OUT}_mosaic.nc"

for n in $(seq 1 "${ntiles}"); do
    ${NCP} "${FIXorog}/${CASE_OUT}/sfc/${CASE_OUT}.mx${OCNRES_OUT}.vegetation_type.tile${n}.nc" \
            "${DATA}/vegetation_type.tile${n}.nc"
    ${NCP} "${FIXorog}/${CASE_OUT}/${CASE_OUT}_grid.tile${n}.nc" \
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
        ${NCP} "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}sfci00${FHR}.nc" \
               "${DATA}/enkfgdas.sfci.nc"

        ${APRUN_REGRID} "${REGRID_EXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"

        for n in $(seq 1 "${ntiles}"); do
            cpfs "${DATA}/sfci.tile${n}.nc"  "${COMOUT_ATMOS_ANALYSIS_MEM}/sfci00${FHR}.tile${n}.nc" 
        done
    done
done

exit 0

