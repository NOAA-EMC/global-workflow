#! /usr/bin/env bash                                                                                                                                                                    

################################################################################
## UNIX Script Documentation Block
## Script name:         exglobal_extractvars.sh
## Script description:  Extracts variables from atmosphere, ocean, ice and wave
##                      products and saves these variables in arcdir
#######################
# Main body starts here
#######################

source "${USHgfs}/preamble.sh"
source "${USHgfs}/extractvars_tools.sh"

# Scripts used
EXTRCTVARA="${USHgfs}/atmos_extractvars.sh"
EXTRCTVARO="${USHgfs}/ocnice_extractvars.sh"
EXTRCTVARW="${USHgfs}/wave_extractvars.sh"

# Set FHMAX_HF_GFS equal to FHMAX_GFS if FHMAX_HF_GFS is greater than FHMAX_GFS
if (( FHMAX_GFS < FHMAX_HF_GFS )); then
  export FHMAX_HF_GFS=${FHMAX_GFS}
fi

# Set FHOUT_WAV_EXTRACT equal to FHOUT_WAV if FHOUT_WAV is not a factor of FHOUT_WAV_EXTRACT
if (( FHOUT_WAV_EXTRACT % FHOUT_WAV != 0 )); then
  FHOUT_WAV_EXTRACT=${FHOUT_WAV}
fi

# Extract variables for atmosphere
if [[ "${DO_ATM}" == "YES" ]]; then
  ${EXTRCTVARA} "${DATA}/atmos"
fi

# Extract variables for ocean
if [[ "${DO_OCN}" == "YES" ]]; then
  export component_name="ocn"
  ${EXTRCTVARO} "${DATA}/ocn" "${varlist_ocn_netcdf}" "${ocnres}" "${compress_ocn}" "${FHOUT_OCN_GFS}" "${ARC_RFCST_PROD_OCN}"
fi

# Extract variables for ice
if [[ "${DO_ICE}" == "YES" ]]; then
  export component_name="ice"
  ${EXTRCTVARO} "${DATA}/ice" "${varlist_ice_netcdf}" "${iceres}" "${compress_ice}" "${FHOUT_ICE_GFS}" "${ARC_RFCST_PROD_ICE}"
fi

# Extract variables for wave
if [[ "${DO_WAVE}" == "YES" ]]; then
  export component_name="wav"
  ${EXTRCTVARW} "${DATA}/wav"
fi

exit 0
