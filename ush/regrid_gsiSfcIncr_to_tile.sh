#! /usr/bin/env bash

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

n_vars=$(( LSOIL_INCR*2 ))

soil_incr_vars=""
for vi in $( seq 1 "${LSOIL_INCR}" ); do
    soil_incr_vars=${soil_incr_vars}'"soilt'${vi}'_inc"',
done
for vi in $( seq 1 "${LSOIL_INCR}" ); do
    soil_incr_vars=${soil_incr_vars}'"slc'${vi}'_inc"',
done

n_tims=1
if [[ ${DO_LAND_IAU} = ".true." ]]; then
    n_tims=0
    ifhrs=()
    ifhrsi=()
    ifhrsf=()
    IFS=',' read -ra landifhrs <<< "${LAND_IAU_FHRS}"  
    for ihr in "${landifhrs[@]}"; do
        hrstr="$(printf "%02d" "${ihr}")";
	ifhrsi+=("${hrstr}");
        ifhrs+=("\"${hrstr}\",");
        n_tims=$((n_tims+1));
	hrsf="$(printf "%.1f" "${ihr}")";
	ifhrsf+=("${hrsf}");        
    done
fi

#time_list="${ifhrs[@]}"
in_fname="enkfgdas.sfci.nc"
out_fname="sfci"

cat << EOF > regrid.nml_tmpl
 &config
  n_vars=${n_vars},
  variable_list=${soil_incr_vars}
  missing_value=0.,
! n_tims=${n_tims},
! time_list=${ifhrs[@]} 
 /
 &input
  gridtype="gau_inc",
  ires=${LONB_CASE_IN},
  jres=${LATB_CASE_IN},
  fname=${in_fname}, 
  dir="./",
  fname_coord="gaussian_scrip.nc",
  dir_coord="./"
/

 &output
  gridtype="fv3_rst",
  ires=${CASE_OUT:1},
  jres=${CASE_OUT:1},
  fname=${out_fname},
  dir="./",
  fname_mask="vegetation_type" 
  dir_mask="./"
  dir_coord="./",
 /
EOF

# input, fixed files
${NCP} "${FIXorog}/${CASE_IN}/gaussian.${LONB_CASE_IN}.${LATB_CASE_IN}.nc" \
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
    
    rm -f "regrid.nml"
    ${NCP} "regrid.nml_tmpl" "regrid.nml"

    for FHR in "${soilinc_fhrs[@]}"; do
        ${NCP} "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}sfci00${FHR}.nc" \
               "${DATA}/${in_fname}"

        ${APRUN_REGRID} "${REGRID_EXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"

        for n in $(seq 1 "${ntiles}"); do
            cpfs "${DATA}/sfci.tile${n}.nc"  "${COMOUT_ATMOS_ANALYSIS_MEM}/sfci00${FHR}.tile${n}.nc"
        done
    done

    if [[ ${DO_LAND_IAU} = ".true." ]]; then 
        
        sed -i -e 's/!/ /g' "regrid.nml"
        
	      #TODO: fix until reg code time dim issues are sorted out
        if [[ "${n_tims}" -eq 1 ]]; then
            for FHI in "${ifhrsi[@]}"; do
                ${NCP} "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}sfci0${FHI}.nc" \
                       "${DATA}/${in_fname}"
            done
        else
            for FHI in "${ifhrsi[@]}"; do
	              ${NCP} "${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}sfci0${FHI}.nc" \
                       "${DATA}/${in_fname}.${FHI}"
            done
        fi
        
	${APRUN_REGRID} "${REGRID_EXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"
   
        #TODO: fix until reg code time dim issues are sorted out
	if [[ "${n_tims}" -eq 1 ]]; then 
            for n in $(seq 1 "${ntiles}"); do
                ncecat -O -u Time "sfci.tile${n}.nc" "sfci.tile${n}.nc"   
                ncap2 -A -s @all="{${ifhrsf[*]}}" "sfci.tile${n}.nc" "sfci.tile${n}.nc"
                ncap2 -O -s'Time[Time]=@all' "sfci.tile${n}.nc" "sfci.tile${n}.nc"
            done	
        fi

        for n in $(seq 1 "${ntiles}"); do
            cpfs "${DATA}/sfci.tile${n}.nc"  "${COMOUT_ATMOS_ANALYSIS_MEM}/sfc_inc.tile${n}.nc"
        done
        
    fi

done

exit 0

