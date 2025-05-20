#!/usr/bin/env bash

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

source "${USHgfs}/atparse.bash"  # include function atparse for parsing @[XYZ] templated files

#------------------------------------------------------------------------
# The snow2mdl executable and namelist
#------------------------------------------------------------------------

SNOW2MDLEXEC=${SNOW2MDLEXEC:-"${EXECgfs}/emcsfc_snow2mdl"}
SNOW2MDLNMLTMPL=${SNOW2MDLNMLTMPL:-"${PARMgfs}/prep_sfc/snow2mdl.nml.tmpl"}

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
IMS_FILE=${IMS_FILE:-"imssnow96.grib2"}

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
# Do a quick check of the ims data to ensure it exists and is not corrupt.
# If available, copy to DATA.
#------------------------------------------------------------------------

if [[ -f ${IMS_FILE} ]]; then
  cpreq "${IMS_FILE}" "${DATA}/imssnow96.grib2"
else
  echo "WARNING: Missing IMS data. Will not run ${SNOW2MDLEXEC}."
  exit 7
fi

#------------------------------------------------------------------------
# The model analysis time is set to the ims valid time, because the
# ims data has highest priority of all input data.
#------------------------------------------------------------------------

${WGRIB2} -d 1 "imssnow96.grib2"
err=$?
if [[ ${err} -ne 0 ]]; then
  echo "WARNING: Corrupt IMS data. Will not run ${SNOW2MDLEXEC}."
  exit 9
else
  tempdate=$(${WGRIB2} -t "imssnow96.grib2" | head -1) || true
  IMSDATE=${tempdate#*d=}
fi

#------------------------------------------------------------------------
# Ensure AFWA data exists and is not too old.
# If available, copy to DATA.
#------------------------------------------------------------------------

if [[ ! -f ${AFWA_GLOBAL_FILE} ]]; then
  echo "WARNING: Missing AFWS data. Will not run ${SNOW2MDLEXEC}."
  exit 3
else
  cpreq "${AFWA_GLOBAL_FILE}" "${DATA}/snow.usaf.grib2"
  ${WGRIB2} -d 1 "snow.usaf.grib2"
  err=$?
  if [[ ${err} -ne 0 ]]; then
    echo "WARNING: Corrupt AFWS data. Will not run ${SNOW2MDLEXEC}."
    exit "${err}"
  else
    tempdate=$(${WGRIB2} -d 1 -t "snow.usaf.grib2")
    AFWADATE=${tempdate#*d=}
    two_days_ago=$(date --utc -d "${IMSDATE:0:8} ${IMSDATE:8:2} - 48 hours" +%Y%m%d%H)
    if [[ ${AFWADATE} -lt ${two_days_ago} ]]; then
      echo "WARNING: Found old AFWA data. Will not run ${SNOW2MDLEXEC}."
      exit 4
    fi
  fi
fi

#------------------------------------------------------------------------
# Additional variables used in the namelist for &output_grib_time
#------------------------------------------------------------------------
export IMSYEAR=${IMSDATE:0:4}
export IMSMONTH=${IMSDATE:4:2}
export IMSDAY=${IMSDATE:6:2}
export IMSHOUR=0   # emc convention is to use 00Z.

# Render the namelist template
if [[ ! -f "${SNOW2MDLNMLTMPL}" ]]; then
  echo "FATAL ERROR: template '${SNOW2MDLNMLTMPL}' does not exist, ABORT!"
  exit 1
fi
rm -f ./fort.41
atparse < "${SNOW2MDLNMLTMPL}" >> "./fort.41"
echo "Rendered fort.41"
cat "./fort.41"

export pgm="emcsfc_snow2mdl"
pgmout=${pgmout:-"OUTPUT"}

source prep_step

"${SNOW2MDLEXEC}" >> "${pgmout}" 2> errfile
err=$?

if [[ ${err} -ne 0 ]]; then
  echo "WARNING: ${pgm} completed abnormally."
  exit "${err}"
else
  echo "${pgm} completed normally."
  cpfs "${MODEL_SNOW_FILE}" "${COMOUT_OBS}"
  rm -f "${MODEL_SNOW_FILE}"
fi

rm -f ./fort.41

exit 0
