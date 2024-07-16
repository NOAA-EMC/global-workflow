#! /usr/bin/env bash                                                                                                                                                                    
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"
source "${USHgfs}/extractvars_tools.sh"

# Scripts used
EXTRCTVARA=${EXTRCTVARA:-"${USHgfs}/atmos_extractvars.sh"}
EXTRCTVARO=${EXTRCTVARO:-"${USHgfs}/ocnice_extractvars.sh"}
EXTRCTVARW=${EXTRCTVARW:-"${USHgfs}/wave_extractvars.sh"}

# Define a job-specific variable for FHMAX_HF_GFS
if (( FHMAX_GFS < FHMAX_HF_GFS )); then
  export FHMAX_HF_EV=FHMAX_GFS
else
  export FHMAX_HF_EV=FHMAX_HF_GFS
fi

# Set FHOUT_WAV_EXTRACT equal to FHOUT_WAV if FHOUT_WAV is not a factor of FHOUT_WAV_EXTRACT
if (( FHOUT_WAV_EXTRACT % FHOUT_WAV != 0 )); then
  FHOUT_WAV_EXTRACT=${FHOUT_WAV}
fi

# Extract variables for atmosphere
if [[ "${DO_ATM}" == "YES" ]];then
  if [[ ! -d "${DATA}/mem${ENSMEM}_atmos" ]]; then 
    mkdir -p "${DATA}/mem${ENSMEM}_atmos" 
  fi
  ${EXTRCTVARA} "${DATA}/mem${ENSMEM}_atmos"
fi

# Extract variables for ocean
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

# Extract variables for ice
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

# Extract variables for wave
if [[ "${DO_WAVE}" == "YES" ]];then
  export component_name="wav"
  if [[ ! -d "${DATA}/mem${ENSMEM}_wav" ]]; then 
    mkdir -p "${DATA}/mem${ENSMEM}_wav" 
  fi
  ${EXTRCTVARW} "${DATA}/mem${ENSMEM}_wav"
fi

exit 0
