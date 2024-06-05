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

#Paramater Tables used
export varlist_2d=${varlist_2d:-"${PARMgfs}/product/gefs_shortparmlist_2d.parm"} #Parameter table for surface variables
export varlist_3d=${varlist_3d:-"${PARMgfs}/product/gefs_shortparmlist_3d_h.parm"} #Parameter table for upper air instantaneous variables
export varlist_3d_d=${varlist_3d_d:-"${PARMgfs}/product/gefs_shortparmlist_3d_d.parm"} #Parameter table for upper air daily-averaged variables
export varlist_wav=${varlist_wav:-"${PARMgfs}/product/gefs_wav_shortparmlist.parm"} #Parameter table for wave variables
export varlist_ocn_netcdf=${varlist_ocn_netcdf:-"${PARMgfs}/product/gefs_ocn_shortparmlist.parm"} #Parameter table for wave variables
export varlist_ice_netcdf=${varlist_ice_netcdf:-"${PARMgfs}/product/gefs_ice_shortparmlist.parm"} #Parameter table for wave variables
export varlist_ocn_grib2=${varlist_ocn_grib2:-"${PARMgfs}/product/gefs_ocn_shortparmlist_grib2.parm"} #Parameter table for wave variables
export varlist_ice_grib2=${varlist_ice_grib2:-"${PARMgfs}/product/gefs_ice_shortparmlist_grib2.parm"} #Parameter table for wave variables  

# Variables used in this job
export FHMIN=${FHMIN:-0} #The first lead hour
export FHMAX=${FHMAX:-120} #The total number of lead hours
export FHMAXHF=${FHMAXHF:-60} #The maximum lead hour that will use a high output frequency  
export FHOUTHF=${FHOUTHF:-6} #The high output frequency (in hours)
export FHOUTLF=${FHOUTLF:-12} #The low output frequency (in hours)
export cycle=${cycle:-"t00z"} #cycle
export ocn_dataformat=${ocn_dataformat:-"grib2"} #the data format of the ocean products to be read
export ice_dataformat=${ice_dataformat:-"grib2"} #the data format of the ice products to be read
export ocnres=${ocnres:-"5p00"} #Resolution of ocean products
export iceres=${iceres:-"5p00"} #Resolution of ice products
export ocnres=${ocnres:-"5p00"} #Resolution of ocean products
export wavinres=${wavinres:-"5p00"} #Resolution of wave products
export wavoures=${wavoures:-"1p00"} #Resolution of wave products
export compress_ocn=${compress_ocn:-1} #1: compress extracted ocean product, 0: do not compress extracted ocean product
export compress_ice=${compress_ice:-1} #1: compress extracted ice product, 0: do not compress extracted ice product 
export FHOUT_WAV_NOSCRUB=${FHOUT_WAV_NOSCRUB:-6} #Frequency of wave output to be saved on disk

#Extract variables for atmosphere
if [[ ! -d "${DATA}/mem${ENSMEM}_atmos" ]]; then 
  mkdir -p "${DATA}/mem${ENSMEM}_atmos" 
fi
${EXTRCTVARA} "${ENSMEM}" "${DATA}/mem${ENSMEM}_atmos"

#Extract variables for ocean
export component_name="ocn"
if [[ ! -d "${DATA}/mem${ENSMEM}_ocn" ]]; then 
  mkdir -p "${DATA}/mem${ENSMEM}_ocn" 
fi
${EXTRCTVARO} "${DATA}/mem${ENSMEM}_ocn" "${varlist_ocn_grib2}" "${ocn_dataformat}" "${ocnres}" "${compress_ocn}"

#Extract variables for ice
export component_name="ice"
if [[ ! -d "${DATA}/mem${ENSMEM}_ice" ]]; then 
  mkdir -p "${DATA}/mem${ENSMEM}_ice" 
fi                                                                                                                                                                                                                                       
${EXTRCTVARO} "${DATA}/mem${ENSMEM}_ice" "${varlist_ice_grib2}" "${ice_dataformat}" "${iceres}" "${compress_ice}"

#Extract variables for wave
export component_name="wav"
if [[ ! -d "${DATA}/mem${ENSMEM}_wav" ]]; then 
  mkdir -p "${DATA}/mem${ENSMEM}_wav" 
fi
${EXTRCTVARW} "${ENSMEM}" "${DATA}/mem${ENSMEM}_wav"

exit 0
