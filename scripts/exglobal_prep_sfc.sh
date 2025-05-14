#!/bin/bash

####  UNIX Script Documentation Block ###################################
#                      .                                             .
# Script name:  exglobal_prep_sfc.sh
# RFC Contact:  George Gayno
# Abstract:  This script calls two utility scripts to prepare a global
#    blended ice analysis and global snow analyses for use by GFS/GDAS.
#    If there is an error in either utility script, the ice/snow
#    file from the previous cycle is copied to the current com
#    directory.  If the ice/snow file from the previous cycle is
#    unavailable, the script aborts.
#
# Script History Log:
#    07/2014  Gayno   Initial version
#    12/2014  Gayno   Set file defaults to grib 2 versions
#                     when available.
#    08/2015  Gayno   Update to current NCO standards
#    08/2020  Gayno   Rename without the '.ecf' extention per
#                     latest NCO standards.
#
# Usage:
#  Parameters:    < no arguments >
#  Modules:
#    ush/prep_sfc_ice_blend.sh (create global ice blend)
#    ush/prep_sfc_snow.sh (create model snow analysis)
#  Input Files:
#    $AFWA_GLOBAL_FILE       - afwa snow data (grib 2)
#    $IMS_FILE               - nh ims snow cover and ice data (grib 2)
#    $FIVE_MIN_ICE_FILE      - global 5-minute ice concentration (grib 2)
#    $FIVE_MIN_ICE_MASK_FILE - corresponding land/sea mask for $FIVE_MIN_ICE_FILE
#                              (grib 2)
#    $BLENDED_ICE_FILE_m6hrs - global 5-minute blended ice data (grib 1)
#                              from previous cycle.  Backup data if
#                              program failure.
#    $MODEL_SLMASK_FILE      - model land/sea mask (grib 1)
#    $MODEL_LATITUDE_FILE    - model latitudes (grib 1)
#    $MODEL_LONGITUDE_FILE   - model longitudes (grib 1)
#    $GFS_LONSPERLAT_FILE    - gfs reduced grid information (text)
#    $MODEL_SNOW_FILE_m6hrs  - snow analysis on model grid (grib 1)
#                              from previous cycle.  Backup data if
#                              program failure
#  Output Files:
#    $BLENDED_ICE_FILE - global 5-minute blended ice data (grib 1)
#    $MODEL_SNOW_FILE  - snow analysis on model grid (grib 1)
#
# Condition codes:
#  0       - normal termination
#  non-0   - problem in the prep_sfc_ice_blend.sh or prep_sfc_snow.sh
#            script and the backup ice or snow data is missing.
#            fatal error.
#
# Attributes:
#     Language:  RedHat Linux
#     Machine:   NCEP WCOSS
#
#########################################################################

##############################################################
# Input data for emcsfc_ice_blend and emcsfc_snow2mdl programs
##############################################################

# ims snow cover and ice cover data (grib 1 or grib 2)
export IMS_FILE="${COMINobsproc}/${RUN}.t${cyc}z.imssnow96.grib2"

# global 5-minute ice concentration file (grib 2)
export FIVE_MIN_ICE_FILE="${COMINobsproc}/${RUN}.t${cyc}z.seaice.5min.grib2"

# landmask file for global 5-minute data (grib 2)
export FIVE_MIN_ICE_MASK_FILE="${FIXgfs}/am/emcsfc_gland5min.grib2"

# afwa snow depth data
export AFWA_GLOBAL_FILE="${COMINobsproc}/${RUN}.t${cyc}z.snow.usaf.grib2"
export AFWA_NH_FILE="${COMINobsproc}/${RUN}.t${cyc}z.NPR.SNWN.SP.S1200.MESH16.grb"
export AFWA_SH_FILE="${COMINobsproc}/${RUN}.t${cyc}z.NPR.SNWS.SP.S1200.MESH16.grb"

# the output ice blend data (grib)
export BLENDED_ICE_FILE="${RUN}.t${cyc}z.seaice.5min.blend.grb"

# the 6-hour old output ice blend data.
export BLENDED_ICE_FILE_PREV="${COMINobsproc_PREV}/${RUN}.t${gcyc}z.seaice.5min.blend.grb"

# the emcsfc_ice_blend executable
export BLENDICEEXEC=${BLENDICEEXEC:-${EXECgfs}/emcsfc_ice_blend}

# standard output file
export pgmout=${pgmout:-OUTPUT}

#-----------------------------------------------------------------------
# call utility script to create global ice blend data.
#-----------------------------------------------------------------------

echo "Create blended ice data."
"${USHgfs}/prep_sfc_ice_blend.sh"
export err=$?

#-----------------------------------------------------------------------
# If there is a failure in the ice blend script, copy the 6-hr old file
# to the current file.  The gfs/gdas can run with old ice data for
# about a week.  Although not fatal, any errors must be
# investigated.
#
# If there is a failure in the ice blend script AND the 6-hour old file
# is not available as a backup, abort the script.  The global cycling
# code can't run without an ice analysis.
#-----------------------------------------------------------------------

if [[ ${err} -ne 0 ]]; then
  if [[ -s "${BLENDED_ICE_FILE_PREV}" ]]; then
    echo "Copy old ice blend file to current directory"
    cpfs "${BLENDED_ICE_FILE_PREV}" "${COMOUT_OBS}/${BLENDED_ICE_FILE}"
    export err=0
  else
    err_exit "FATAL ERROR: CURRENT ICE FILE AND 6-HR OLD ICE FILE MISSING"
  fi
fi

#-----------------------------------------------------------------------
# now create global snow depth data for full-res gaussian grid
# and (if a gdas run) enkf gaussian grid.
#-----------------------------------------------------------------------

export SNOW2MDLEXEC="${EXECgfs}/emcsfc_snow2mdl"

LONB_CASE=$((4*${CASE:1}))
LATB_CASE=$((2*${CASE:1}))

export MODEL_SLMASK_FILE=${SLMASK:-${FIXgfs}/am/global_slmask.t${CASE:1}.${LONB_CASE}.${LATB_CASE}.grb}
export MODEL_LATITUDE_FILE=${MDL_LATS:-${FIXgfs}/am/global_latitudes.t${CASE:1}.${LONB_CASE}.${LATB_CASE}.grb}
export MODEL_LONGITUDE_FILE=${MDL_LONS:-${FIXgfs}/am/global_longitudes.t${CASE:1}.${LONB_CASE}.${LATB_CASE}.grb}
export GFS_LONSPERLAT_FILE=${LONSPERLAT:-${FIXgfs}/am/global_lonsperlat.t${CASE:1}.${LONB_CASE}.${LATB_CASE}.txt}
export MODEL_SNOW_FILE=${RUN}.t${cyc}z.snogrb_t${CASE:1}.${LONB_CASE}.${LATB_CASE}
export MODEL_SNOW_FILE_PREV=${COMINobsproc_PREV}/${RUN}.t${gcyc}z.snogrb_t${CASE:1}.${LONB_CASE}.${LATB_CASE}

echo "Create ${CASE} snow data."
"${USHgfs}/prep_sfc_snow.sh"
export err=$?

#----------------------------------------------------------------------
# If there was a failure in the prep_sfc_snow script, copy the 6-hr old
# snow file to the current file.  The gfs/gdas can run with old snow data
# for a day or two at most.   So while not fatal, any errors must be
# investigated
#
# If there is a failure in the prep_sfc_snow script AND the 6-hour old
# snow file is not available as a backup, abort the script.  The
# global cycling can't run without an snow analysis.
#-----------------------------------------------------------------------

if [[ ${err} -ne 0 ]]; then
  if [[ -s "${MODEL_SNOW_FILE_PREV}" ]]; then
    echo "COPY OLD ${CASE} SNOW FILE TO CURRENT DIRECTORY"
    cpfs "${MODEL_SNOW_FILE_PREV}" "${COMOUT_OBS}/${MODEL_SNOW_FILE}"
    export err=0
  else
    err_exit "CURRENT AND 6-HR OLD ${CASE} SNOW MISSING, ABORT!"
  fi  # check of missing 6-hr snow file
fi  # return code check

#-----------------------------------------------------------------------
# Create enkf snow file if EUPD_CYC is RUN or BOTH
#-----------------------------------------------------------------------

if [[ "${EUPD_CYC}" = "${RUN}" ]] || [[ "${EUPD_CYC^^}" = "BOTH" ]]; then

  LONB_CASE_ENS=$((4*${CASE_ENS:1}))
  LATB_CASE_ENS=$((2*${CASE_ENS:1}))

  export MODEL_SLMASK_FILE=${SLMASK_ENKF:-${FIXgfs}/am/global_slmask.t${CASE_ENS:1}.${LONB_CASE_ENS}.${LATB_CASE_ENS}.grb}
  export MODEL_LATITUDE_FILE=${MDL_LATS_ENKF:-${FIXgfs}/am/global_latitudes.t${CASE_ENS:1}.${LONB_CASE_ENS}.${LATB_CASE_ENS}.grb}
  export MODEL_LONGITUDE_FILE=${MDL_LONS_ENKF:-${FIXgfs}/am/global_longitudes.t${CASE_ENS:1}.${LONB_CASE_ENS}.${LATB_CASE_ENS}.grb}
  export GFS_LONSPERLAT_FILE=${LONSPERLAT_ENKF:-${FIXgfs}/am/global_lonsperlat.t${CASE_ENS:1}.${LONB_CASE_ENS}.${LATB_CASE_ENS}.txt}
  export MODEL_SNOW_FILE=${RUN}.t${cyc}z.snogrb_t${CASE_ENS:1}.${LONB_CASE_ENS}.${LATB_CASE_ENS}
  export MODEL_SNOW_FILE_PREV=${COMINobsproc_PREV}/${RUN}.t${gcyc}z.snogrb_t${CASE_ENS:1}.${LONB_CASE_ENS}.${LATB_CASE_ENS}

  echo "Create enkf snow data."
  "${USHgfs}/prep_sfc_snow.sh"
  export err=$?

  #-----------------------------------------------------------------------
  # Check for errors creating enkf snow.  Use 6-hour old data
  # as backup.  If old data not available, abort.
  #-----------------------------------------------------------------------

  if [[ ${err} -ne 0 ]]; then
    if [[ -s "${MODEL_SNOW_FILE_PREV}" ]]; then
      echo "COPY OLD ENKF SNOW FILE TO CURRENT DIRECTORY"
      cpfs "${MODEL_SNOW_FILE_PREV}" "${COMOUT_OBS}/${MODEL_SNOW_FILE}"
      export err=0
    else
      err_exit "CURRENT AND 6-HR OLD ENKF SNOW MISSING"
    fi  # check of missing 6-hr snow file
  fi  # return code check

fi # If ENKF runs for RUN

exit 0
