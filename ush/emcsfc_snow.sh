#!/bin/ksh

####  UNIX Script Documentation Block ###################################
#                      .                                             .
# Script name:  emcsfc_snow.sh
# RFC Contact:  George Gayno
# Abstract:  This script calls the emcsfc_snow2mdl program to create a 
#    model snow analysis from IMS snow cover and AFWA snow depth data.
#
# Script History Log:
#    07/2014  Gayno   Initial version
#    12/2014  Gayno   Use grib 2 version of snow cover climo file.
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
#    $AFWA_NH_FILE         - nh afwa snow data (grib 1)
#    $AFWA_SH_FILE         - sh afwa snow data (grib 1)
#    $IMS_FILE             - nh ims snow cover data (grib 2)
#    $CLIMO_QC             - nh climatological snow cover (grib 2)
#    fort.41               - program configuration namelist
#  Output files:
#    $MODEL_SNOW_FILE      - output snow analysis on model grid (grib 1 or 2)
#
# Condition codes:
#  0       - normal termination
#  $rc1    - non-zero status indicates corrupt ims data.
#  $rc2    - non-zero status indicates a problem in emcsfc_snow2mdl execution.
#            see source code for details - /nwprod/gfs.vX.Y.Z/sorc/emcsfc_snow2mdl.fd
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

VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" == YES ]]; then
  set -x
fi

#-----------------------------------------------------------------------
# the "postmsg", "startmsg" and "prep_step" utilities
# are only used in ncep ops when the "prod_util" module is loaded.
#-----------------------------------------------------------------------

use_prod_util=`echo $UTILROOT`
if ((${#use_prod_util} != 0)); then
  use_prod_util="true"
else
  use_prod_util="false"
fi

export pgm=emcsfc_snow2mdl
if test "$use_prod_util" = "true" ; then
  startmsg
fi

#------------------------------------------------------------------------
# Path names
#------------------------------------------------------------------------

envir=${envir:-"prod"}
NWROOT=${NWROOT:-"/nw${envir}"}
HOMEgfs=${HOMEgfs:-$NWROOT/gfs.${gfs_ver:?}}
EXECgfs=${EXECgfs:-$HOMEgfs/exec}
FIXam=${FIXam:-$HOMEgfs/fix/fix_am}

COMOUT=${COMOUT:-$PWD}

#------------------------------------------------------------------------
# The snow2mdl executable
#------------------------------------------------------------------------

SNOW2MDLEXEC=${SNOW2MDLEXEC:-${EXECgfs}/emcsfc_snow2mdl}

#------------------------------------------------------------------------
# Fixed files that describe the model grid: landmask, latitudes/longitudes.
# And for gfs only, the definition of the reduced grid (lonsperlat).
# The lonsperlat file is optional.  If not chosen, will create gfs
# snow analysis on the 'full' grid.
#------------------------------------------------------------------------

MODEL_SLMASK_FILE=${MODEL_SLMASK_FILE:-global_slmask.t1534.3072.1536.grb}
MODEL_LATITUDE_FILE=${MODEL_LATITUDE_FILE:-global_latitudes.t1534.3072.1536.grb}
MODEL_LONGITUDE_FILE=${MODEL_LONGITUDE_FILE:-global_longitudes.t1534.3072.1536.grb}
GFS_LONSPERLAT_FILE=${GFS_LONSPERLAT_FILE:-global_lonsperlat.t1534.3072.1536.txt}

#------------------------------------------------------------------------
# Input snow data.  ims snow cover and afwa snow depth. ims is NH only.
# In OPS, we run with ims only, or ims and afwa.  afwa data is too
# unreliable to use on its own.  ims is grib2.  afwa is grib1.
#------------------------------------------------------------------------

AFWA_NH_FILE=${AFWA_NH_FILE:-"NPR.SNWN.SP.S1200.MESH16"}
AFWA_SH_FILE=${AFWA_SH_FILE:-"NPR.SNWS.SP.S1200.MESH16"}
IMS_FILE=${IMS_FILE:-"imssnow96.grb.grib2"}

#------------------------------------------------------------------------
# File of snow cover climo used to qc the input snow data
#------------------------------------------------------------------------

CLIMO_QC=${CLIMO_QC:-${FIXam}/emcsfc_snow_cover_climo.grib2}

#------------------------------------------------------------------------
# Output snow analysis on model grid
#------------------------------------------------------------------------

MODEL_SNOW_FILE=${MODEL_SNOW_FILE:-"snogrb_model"}
OUTPUT_GRIB2=${OUTPUT_GRIB2:-.false.}  # grib 1 when false.

SENDCOM=${SENDCOM:-NO}

#------------------------------------------------------------------------
# working directory
#------------------------------------------------------------------------

DATA=${DATA:-$(pwd)}
if [ ! -d $DATA ]; then
  mkdir -p $DATA
fi
cd $DATA

#------------------------------------------------------------------------
# Do a quick check of the ims data to ensure it is not corrupt.
# WGRIB2 works for a grib 1 or grib 2 file.  If IMS is bad,
# don't run emcsfc_snow2mdl program because afwa data alone is
# unreliable.
#------------------------------------------------------------------------

$WGRIB2 ${IMS_FILE}
rc1=$?

if ((rc1 != 0));then 
  msg="WARNING: ${pgm} detects corrupt IMS data. Can not run."
  if test "$use_prod_util" = "true" ; then
    postmsg "$jlogfile" "$msg"
  fi
  exit $rc1
fi

#------------------------------------------------------------------------
# The model analysis time is set to the ims valid time, because the
# ims data has highest priority of all input data.
#------------------------------------------------------------------------

$WGRIB2 -Sec0 ${IMS_FILE} 2>&1 | grep "grib1 message"
status=$?
if (( status == 0 )); then   # grib 1 file
  tempdate=$($WGRIB -v $IMS_FILE | head -1)
  typeset -L10 IMSDATE10
  IMSDATE10=${tempdate#*D=}
else # grib 2 file
  tempdate=$($WGRIB2 -t $IMS_FILE | head -1)
  typeset -L10 IMSDATE10
  IMSDATE10=${tempdate#*d=}
fi
IMSYEAR=$(echo $IMSDATE10 | cut -c1-4)
IMSMONTH=$(echo $IMSDATE10 | cut -c5-6)
IMSDAY=$(echo $IMSDATE10 | cut -c7-8)
IMSHOUR=0   # emc convention is to use 00Z.

if test "$use_prod_util" = "true" ; then
  . prep_step
fi

rm -f ./fort.41
cat > ./fort.41 << !
 &source_data
  autosnow_file=""
  nesdis_snow_file="${IMS_FILE}"
  nesdis_lsmask_file=""
  afwa_snow_global_file=""
  afwa_snow_nh_file="${AFWA_NH_FILE}"
  afwa_snow_sh_file="${AFWA_SH_FILE}"
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

pgmout=${pgmout:-OUTPUT}

eval $SNOW2MDLEXEC  >> $pgmout 2> errfile
rc2=$?

if ((rc2!= 0));then 
  msg="WARNING: ${pgm} completed abnormally."
  if test "$use_prod_util" = "true" ; then
    postmsg "$jlogfile" "$msg"
  fi
  exit $rc2
else
  msg="${pgm} completed normally."
  if test "$use_prod_util" = "true" ; then
    postmsg "$jlogfile" "$msg"
  fi
  if test "$SENDCOM" = "YES"
  then
    cp $MODEL_SNOW_FILE  $COMOUT
    rm -f $MODEL_SNOW_FILE
  fi
fi

rm -f ./fort.41

exit 0
