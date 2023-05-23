#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## CICE5/MOM6 post driver script
## FHRGRP : forecast hour group to post-process (e.g. 0, 1, 2 ...)
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="ocnpost"
export jobid="${job}.$$"
source "${HOMEgfs}/ush/jjob_header.sh" -e "ocnpost" -c "base ocnpost"

##############################################
# Set variables used in the exglobal script
##############################################
export CDUMP=${RUN/enkf}
if [[ ${RUN_ENVIR} = "nco" ]]; then
    export ROTDIR=${COMROOT:?}/${NET}/${envir}
fi

##############################################
# Begin JOB SPECIFIC work
##############################################
YMD=${PDY} HH=${cyc} generate_com -rx COM_OCEAN_HISTORY COM_OCEAN_2D COM_OCEAN_3D \
  COM_OCEAN_XSECT COM_ICE_HISTORY

for grid in "0p50" "0p25"; do
  YMD=${PDY} HH=${cyc} GRID=${grid} generate_com -rx "COM_OCEAN_GRIB_${grid}:COM_OCEAN_GRIB_TMPL"
done

for outdir in COM_OCEAN_2D COM_OCEAN_3D COM_OCEAN_XSECT COM_OCEAN_GRIB_0p25 COM_OCEAN_GRIB_0p50; do
  if [[ ! -d "${!outdir}" ]]; then
    mkdir -p "${!outdir}"
  fi
done

fhrlst=$(echo ${FHRLST} | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

export OMP_NUM_THREADS=1
export ENSMEM=${ENSMEM:-01}

export IDATE=${PDY}${cyc}

for fhr in ${fhrlst}; do
  export fhr=${fhr}  
  # Ignore possible spelling error (nothing is misspelled)
  # shellcheck disable=SC2153
  VDATE=$(${NDATE} "${fhr}" "${IDATE}")
  # shellcheck disable=
  declare -x VDATE
  cd "${DATA}" || exit 2
  if (( fhr > 0 )); then
    # TODO: This portion calls NCL scripts that are deprecated (see Issue #923)
    if [[ "${MAKE_OCN_GRIB:-YES}" == "YES" ]]; then
      export MOM6REGRID=${MOM6REGRID:-${HOMEgfs}}
      "${MOM6REGRID}/scripts/run_regrid.sh"
      status=$?
      [[ ${status} -ne 0 ]] && exit "${status}"

      # Convert the netcdf files to grib2
      export executable=${MOM6REGRID}/exec/reg2grb2.x
      "${MOM6REGRID}/scripts/run_reg2grb2.sh"
      status=$?
      [[ ${status} -ne 0 ]] && exit "${status}"
      ${NMV} "ocn_ice${VDATE}.${ENSMEM}.${IDATE}_0p25x0p25.grb2" "${COM_OCEAN_GRIB_0p25}/"
      ${NMV} "ocn_ice${VDATE}.${ENSMEM}.${IDATE}_0p5x0p5.grb2" "${COM_OCEAN_GRIB_0p50}/"
    fi

    #break up ocn netcdf into multiple files:
    if [[ -f "${COM_OCEAN_2D}/ocn_2D_${VDATE}.${ENSMEM}.${IDATE}.nc" ]]; then
      echo "File ${COM_OCEAN_2D}/ocn_2D_${VDATE}.${ENSMEM}.${IDATE}.nc already exists"
    else
      ncks -x -v vo,uo,so,temp \
        "${COM_OCEAN_HISTORY}/ocn${VDATE}.${ENSMEM}.${IDATE}.nc" \
        "${COM_OCEAN_2D}/ocn_2D_${VDATE}.${ENSMEM}.${IDATE}.nc"
      status=$?
      [[ ${status} -ne 0 ]] && exit "${status}"
    fi
    if [[ -f "${COM_OCEAN_3D}/ocn_3D_${VDATE}.${ENSMEM}.${IDATE}.nc" ]]; then
       echo "File ${COM_OCEAN_3D}/ocn_3D_${VDATE}.${ENSMEM}.${IDATE}.nc already exists"
    else
      ncks -x -v Heat_PmE,LW,LwLatSens,MLD_003,MLD_0125,SSH,SSS,SST,SSU,SSV,SW,cos_rot,ePBL,evap,fprec,frazil,latent,lprec,lrunoff,sensible,sin_rot,speed,taux,tauy,wet_c,wet_u,wet_v \
        "${COM_OCEAN_HISTORY}/ocn${VDATE}.${ENSMEM}.${IDATE}.nc" \
        "${COM_OCEAN_3D}/ocn_3D_${VDATE}.${ENSMEM}.${IDATE}.nc"
      status=$?
      [[ ${status} -ne 0 ]] && exit "${status}"
    fi
    if [[ ${OCNRES} == "025" ]]; then
      #TODO: the following code is hard-coded for OCNRES=025
      if [[ -f "${COM_OCEAN_XSECT}/ocn-temp-EQ_${VDATE}.${ENSMEM}.${IDATE}.nc" ]]; then
        echo "File ${COM_OCEAN_XSECT}/ocn-temp-EQ_${VDATE}.${ENSMEM}.${IDATE}.nc already exists"
      else
        ncks -v temp -d yh,503 -d xh,-299.92,60.03 \
          "${COM_OCEAN_3D}/ocn_3D_${VDATE}.${ENSMEM}.${IDATE}.nc" \
          "${COM_OCEAN_XSECT}/ocn-temp-EQ_${VDATE}.${ENSMEM}.${IDATE}.nc"
        status=$?
        [[ ${status} -ne 0 ]] && exit "${status}"
      fi
      if [[ -f "${COM_OCEAN_XSECT}/ocn-uo-EQ_${VDATE}.${ENSMEM}.${IDATE}.nc" ]]; then
        echo "File ${COM_OCEAN_XSECT}/ocn-uo-EQ_${VDATE}.${ENSMEM}.${IDATE}.nc already exists"
      else
        ncks -v uo -d yh,503 -d xh,-299.92,60.03 \
          "${COM_OCEAN_3D}/ocn_3D_${VDATE}.${ENSMEM}.${IDATE}.nc" \
          "${COM_OCEAN_XSECT}/ocn-uo-EQ_${VDATE}.${ENSMEM}.${IDATE}.nc"
        status=$?
        [[ ${status} -ne 0 ]] && exit "${status}"
      fi
    fi 
  fi 
done

# clean up working folder
if [[ ${KEEPDATA:-"NO"} = "NO" ]] ; then rm -rf "${DATA}" ; fi
###############################################################
# Exit out cleanly


exit 0
