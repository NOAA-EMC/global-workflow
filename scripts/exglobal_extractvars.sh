#! /usr/bin/env bash                                                                                                                                                                    
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"

# Programs used
export WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}

# Scripts used
EXTRCTVARA=${EXTRCTVARA:-"${USHgfs}/gefs_atmos_extractvars.sh"}
EXTRCTVARO=${EXTRCTVARO:-"${USHgfs}/gefs_ocn_extractvars.sh"}
EXTRCTVARW=${EXTRCTVARW:-"${USHgfs}/gefs_wav_extractvars.sh"}

#Paramater Tables used
export varlist_2d=${varlist_2d:-"${PARMgfs}/product/gefs_shortparmlist_2d.parm"} #Parameter table for surface atmos variables
export varlist_3d=${varlist_3d:-"${PARMgfs}/product/gefs_shortparmlist_3d_h.parm"} #Parameter table for upper air instantaneous variables
export varlist_3d_d=${varlist_3d_d:-"${PARMgfs}/product/gefs_shortparmlist_3d_d.parm"} #Parameter table for upper air daily-averaged variables
export varlist_wav=${varlist_wav:-"${PARMgfs}/product/gefs_wav_shortparmlist.parm"} #Parameter table for wave variables

# Variables used in this job
export FHMIN=${FHMIN:-0} #The total number of lead hours
export fhmax=${fhmax:-120} #The total number of lead hours
export FHMAXHF=${FHMAXHF:-60} #The maximum lead hour that will use a high output frequency  
export FHOUTHF=${FHOUTHF:-6} #The high output frequency (in hours)
export FHOUTLF=${FHOUTLF:-12} #The low output frequency (in hours)

subdata="${DATA}/mem${ENSMEM}_atmos"
if [ ! -d ${subdata} ]; then mkdir -p ${subdata}; fi
${EXTRCTVARA} ${ENSMEM} ${subdata}

exit 0
