#!/bin/bash

####  UNIX Script Documentation Block ###################################
#                      .                                             .
# Script name:  prep_sfc_snow.sh
# RFC Contact:  George Gayno
# Abstract:  This script calls the emcsfc_snow2mdl program to create a
#    model snow analysis from IMS snow cover and AFWA snow depth data.
#
# Script History Log:
#    07/2014  Gayno   Initial version
#    12/2014  Gayno   Use grib 2 version of snow cover climo file
#                     Add $OUTPUT_GRIB2 environment variable to
#                     control whether model analysis is grib 1 or
#                     grib 2.
#    08/2015  Gayno   Bring up to current NCO standard.
#
# Usage:
#  Parameters:   < no arguments >
#  Input files:
#    $GFS_LONSPERLAT_FILE  - definition of gfs reduced grid (text)
#    $MODEL_SLMASK_FILE    - model landmask  (grib 1 or 2)
#    $MODEL_LATITUDE_FILE  - model latitude  (grib 1 or 2)
#    $MODEL_LONGITUDE_FILE - model longitude (grib 1 or 2)
#    $AFWA_GLOBAL_FILE     - global afwa snow data (grib 2)
#    $IMS_FILE             - nh ims snow cover data (grib 2)
#    $CLIMO_QC             - nh climatological snow cover (grib 2)
#    fort.41               - program configuration namelist
#  Output files:
#    $MODEL_SNOW_FILE      - output snow analysis on model grid (grib 1 or 2)
#
# Condition codes:
#  0       - normal termination
#  non 0   - indicates missing or corrupt input data
#            or a problem in emcsfc_snow2mdl execution.
#
# If a non-zero status occurs, no model snow analysis will be created.
# This is not fatal to the model executation.  But any problems should
# be investigated.
#
# Attributes:
#     Language:  RedHat Linux
#     Machine:   NCEP WCOSS
#
#########################################################################

export pgm=emcsfc_snow2mdl

#------------------------------------------------------------------------
# The snow2mdl executable
#------------------------------------------------------------------------

SNOW2MDLEXEC=${SNOW2MDLEXEC:-"${EXECgfs}/emcsfc_snow2mdl"}

#------------------------------------------------------------------------
# Fixed files that describe the model grid: landmask, latitudes/longitudes.
# And for gfs only, the definition of the reduced grid (lonsperlat).
# The lonsperlat file is optional.  If not chosen, will create gfs
# snow analysis on the 'full' grid.
#------------------------------------------------------------------------

MODEL_SLMASK_FILE=${MODEL_SLMASK_FILE:-"global_slmask.t1534.3072.1536.grb"}
MODEL_LATITUDE_FILE=${MODEL_LATITUDE_FILE:-"global_latitudes.t1534.3072.1536.grb"}
MODEL_LONGITUDE_FILE=${MODEL_LONGITUDE_FILE:-"global_longitudes.t1534.3072.1536.grb"}
GFS_LONSPERLAT_FILE=${GFS_LONSPERLAT_FILE:-"global_lonsperlat.t1534.3072.1536.txt"}

#------------------------------------------------------------------------
# Input snow data.  ims snow cover and afwa snow depth. ims is NH only.
# AFWA is global.
#------------------------------------------------------------------------

AFWA_GLOBAL_FILE=${AFWA_GLOBAL_FILE:-"snow.usaf.grib2"}
IMS_FILE=${IMS_FILE:-"imssnow96.grb.grib2"}

#------------------------------------------------------------------------
# File of snow cover climo used to qc the input snow data
#------------------------------------------------------------------------

CLIMO_QC=${CLIMO_QC:-"${FIXgfs}/am/emcsfc_snow_cover_climo.grib2"}

#------------------------------------------------------------------------
# Output snow analysis on model grid
#------------------------------------------------------------------------

MODEL_SNOW_FILE=${MODEL_SNOW_FILE:-"snogrb_model"}
OUTPUT_GRIB2=${OUTPUT_GRIB2:-".false."}  # grib 1 when false.

#------------------------------------------------------------------------
# Do a quick check of the ims data to ensure it exists and is
# not corrupt.
#------------------------------------------------------------------------

if [[ ! -f ${IMS_FILE} ]]; then
  echo "WARNING: ${pgm} detects missing ims data. Will not run."
  exit 7
fi

#------------------------------------------------------------------------
# The model analysis time is set to the ims valid time, because the
# ims data has highest priority of all input data.
#------------------------------------------------------------------------

${WGRIB2} -d 1 "${IMS_FILE}"
err=$?
if [[ ${err} -ne 0 ]]; then
  echo "WARNING: ${pgm} detects corrupt ims data. Will not run."
  exit 9
else
  tempdate=$(${WGRIB2} -t "${IMS_FILE}" | head -1)
  IMSDATE=${tempdate#*d=}
fi
IMSDATE10=$(echo "${IMSDATE}" | cut -c1-10)
IMSYEAR=$(echo "${IMSDATE10}" | cut -c1-4)
IMSMONTH=$(echo "${IMSDATE10}" | cut -c5-6)
IMSDAY=$(echo "${IMSDATE10}" | cut -c7-8)
IMSHOUR=0   # emc convention is to use 00Z.

#------------------------------------------------------------------------
# Ensure AFWA data exists and is not too old.
#------------------------------------------------------------------------

if [[ ! -f ${AFWA_GLOBAL_FILE} ]]; then
  echo "WARNING: ${pgm} detects missing afwa data. Will not run."
  exit 3
else
  ${WGRIB2} -d 1 "${AFWA_GLOBAL_FILE}"
  err=$?
  if [[ ${err} -ne 0 ]]; then
    echo "WARNING: ${pgm} detects corrupt afwa data. Will not run."
    exit ${err}
  else
    tempdate=$(${WGRIB2} -d 1 -t "${AFWA_GLOBAL_FILE}")
    AFWADATE=${tempdate#*d=}
    two_days_ago=$(${NDATE} -48 "${IMSDATE10}")
    if [[ ${AFWADATE} -lt ${two_days_ago} ]]; then
      echo "WARNING: ${pgm} detects old afwa data. Will not run."
      exit 4
    fi
  fi
fi

pgmout=${pgmout:-"OUTPUT"}

source prep_step

rm -f ./fort.41
cat > ./fort.41 << !
 &source_data
  autosnow_file=""
  nesdis_snow_file="${IMS_FILE}"
  nesdis_lsmask_file=""
  afwa_snow_global_file="${AFWA_GLOBAL_FILE}"
  afwa_snow_nh_file=""
  afwa_snow_sh_file=""
  afwa_lsmask_nh_file=""
  afwa_lsmask_sh_file=""
 /
 &qc
  climo_qc_file="${CLIMO_QC}"
 /
 &model_specs
  model_lat_file="${MODEL_LATITUDE_FILE}"
  model_lon_file="${MODEL_LONGITUDE_FILE}"
  model_lsmask_file="${MODEL_SLMASK_FILE}"
  gfs_lpl_file="${GFS_LONSPERLAT_FILE}"
  /
 &output_data
  model_snow_file="./${MODEL_SNOW_FILE}"
  output_grib2=${OUTPUT_GRIB2}
 /
 &output_grib_time
  grib_year=${IMSYEAR}
  grib_month=${IMSMONTH}
  grib_day=${IMSDAY}
  grib_hour=${IMSHOUR}
 /
 &parameters
  lat_threshold=55.0
  min_snow_depth=0.05
  snow_cvr_threshold=50.0
 /
!

eval "${SNOW2MDLEXEC}" >> "${pgmout}" 2> errfile
err=$?

if [[ ${err} -ne 0 ]]; then
  echo "WARNING: ${pgm} completed abnormally."
  exit ${err}
else
  echo "${pgm} completed normally."
  cpfs "${MODEL_SNOW_FILE}" "${COMOUT_OBS}"
  rm -f "${MODEL_SNOW_FILE}"
fi

rm -f ./fort.41

exit 0
