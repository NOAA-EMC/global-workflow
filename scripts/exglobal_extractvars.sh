#! /usr/bin/env bash                                                                                                                                                                    
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"

# Programs used
export WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}

# Scripts used
EXTRCTVARA=${EXTRCTVARA:-"${USHgfs}/gefs_atmos_extractvars.sh"}
EXTRCTVARO=${EXTRCTVARO:-"${USHgfs}/gefs_ocnice_extractvars.sh"}
EXTRCTVARW=${EXTRCTVARW:-"${USHgfs}/gefs_wav_extractvars.sh"}

#Check to make sure FHMAX_HF_GFS is less than FHMAX_GFS
if [[ "${FHMAX_GFS}" -lt "${FHMAX_HF_GFS}" ]];then
  echo "FATAL ERROR: FHMAX_GFS (${FHMAX_GFS}) is less than FHMAX_HF_GFS (${FHMAX_HF_GFS}). FHMAX_GFS must be greater than FHMAX_HF_GFS."
  export err=1; err_chk
fi

#Extract variables for atmosphere
if [[ "${DO_ATM}" == "YES" ]];then
  if [[ ! -d "${DATA}/mem${ENSMEM}_atmos" ]]; then 
    mkdir -p "${DATA}/mem${ENSMEM}_atmos" 
  fi
  ${EXTRCTVARA} "${DATA}/mem${ENSMEM}_atmos"
fi

#Extract variables for ocean
if [[ "${DO_OCN}" == "YES" ]];then
  export component_name="ocn"
  if [[ ! -d "${DATA}/mem${ENSMEM}_ocn" ]]; then 
    mkdir -p "${DATA}/mem${ENSMEM}_ocn" 
  fi
  if [[ "${ocn_dataformat}" == "netcdf" ]]; then
    varlist_ocn=${varlist_ocn_netcdf}
  elif [[ "${ocn_dataformat}" == "grib2" ]]; then
    varlist_ocn=${varlist_ocn_grib2}
  else
    echo "FATAL ERROR: Invalid ocean data format provided (${ocn_dataformat})"
    export err=1; err_chk
  fi
  ${EXTRCTVARO} "${DATA}/mem${ENSMEM}_ocn" "${varlist_ocn}" "${ocn_dataformat}" "${ocnres}" "${compress_ocn}" "${FHOUT_OCN_GFS}" "${COMOUT_RFCST_PROD_OCN}"
fi

#Extract variables for ice
if [[ "${DO_ICE}" == "YES" ]];then
  export component_name="ice"
  if [[ ! -d "${DATA}/mem${ENSMEM}_ice" ]]; then 
    mkdir -p "${DATA}/mem${ENSMEM}_ice" 
  fi
  if [[ "${ice_dataformat}" == "netcdf" ]]; then
    varlist_ice=${varlist_ice_netcdf}
  elif [[ "${ice_dataformat}" == "grib2" ]]; then
    varlist_ice=${varlist_ice_grib2}
  else
    echo "FATAL ERROR: Invalid ice data format provided (${ice_dataformat})"
    export err=1; err_chk
  fi
  ${EXTRCTVARO} "${DATA}/mem${ENSMEM}_ice" "${varlist_ice}" "${ice_dataformat}" "${iceres}" "${compress_ice}" "${FHOUT_ICE_GFS}" "${COMOUT_RFCST_PROD_ICE}"
fi

#Extract variables for wave
if [[ "${DO_WAVE}" == "YES" ]];then
  export component_name="wav"
  if [[ ! -d "${DATA}/mem${ENSMEM}_wav" ]]; then 
    mkdir -p "${DATA}/mem${ENSMEM}_wav" 
  fi
  ${EXTRCTVARW} "${DATA}/mem${ENSMEM}_wav"
fi

exit 0
