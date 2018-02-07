#!/bin/sh

####  UNIX Script Documentation Block ###################################
#                      .                                             .
# Script name:  emcsfc_ice_blend.sh
# RFC Contact:  George Gayno
# Abstract:  This script calls the emcsfc_ice_blend program to create 
#    a global ice concentation from a blend of National Ice Center IMS data
#    and EMC/MMAB 5-minute data.
#
# Script History Log:
#    July  2014  Gayno   Initial version
#    Oct   2014  Gayno   The emcsfc_ice_blend program was modified to
#                        use all grib 2 for i/o.  Modify default 
#                        filenames to use 'grib2' extension.
#    Aug   2015  Gayno   Convert output blended ice file from
#                        grib 2 to grib 1.  Bring up to NCO standards.
#
# Usage:
#    Parameters:    [no arguments]
#    Input Files:
#      $IMS_FILE               - ims ice cover data (grib 1 or 2)
#      $FIVE_MIN_ICE_FILE      - 5-minute global ice concentration (grib 2)
#      $FIVE_MIN_ICE_MASK_FILE - land/sea mask of $FIVE_MIN_ICE_FILE
#                                (grib 2)
#    Output Files:
#      $BLENDED_ICE_FILE - blended ice concentration file. program
#                          produces grib 2.  script converts to
#                          grib 1 expected by gfs.
#
# Condition codes:
#      0 - normal termination
#  non 0 - input data is missing or emcsfc_ice_blend program 
#          terminated abnormally.
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

export pgm=emcsfc_ice_blend
if test "$use_prod_util" = "true" ; then
  startmsg
fi

#------------------------------------------------------------------------
# working directory
#------------------------------------------------------------------------

DATA=${DATA:-$PWD}
if [ ! -d $DATA ]; then
  mkdir -p $DATA
fi
cd $DATA

#------------------------------------------------------------------------
# set up script variables
#------------------------------------------------------------------------

# copy output ice blend data to com?
SENDCOM=${SENDCOM:-"NO"}

envir=${envir:-"prod"}
NWROOT=${NWROOT:-"/nw${envir}"}

HOMEgfs=${HOMEgfs:-${NWPROD:-$NWROOT/gfs.${gfs_ver:?}}}
FIXam=${FIXam:-$HOMEgfs/fix/fix_am}
EXECgfs=${EXECgfs:-$HOMEgfs/exec}
 
# output com directory.
COMOUT=${COMOUT:-$PWD}

# the input data.  ims may be grib1 or grib2.  five_min files are grib 2.
IMS_FILE=${IMS_FILE:-"ims.grib2"} 
FIVE_MIN_ICE_FILE=${FIVE_MIN_ICE_FILE:-"seaice.5min.grib2"} 
FIVE_MIN_ICE_MASK_FILE=${FIVE_MIN_ICE_MASK_FILE:-${FIXam}/emcsfc_gland5min.grib2}

# the output ice blend data (grib)
BLENDED_ICE_FILE=${BLENDED_ICE_FILE:-seaice.5min.blend}
 
# the program executable
BLENDICEEXEC=${BLENDICEEXEC:-$EXECgfs/emcsfc_ice_blend}
 
# standard output file
pgmout=${pgmout:-OUTPUT}

#------------------------------------------------------------------------
# Interpolate ims data to ncep grid 173 (the grid used by mmab 5-minute ice data).
# If ims is grib1 format, convert to grib2.  If ims data is missing, then
# don't run ice blend program.  Copy old blended data to current directory.
#------------------------------------------------------------------------

if [ -f "${IMS_FILE}" ]
then
  $WGRIB2 -Sec0 ${IMS_FILE} 2>&1 | grep "grib1 message"
  status=$?
  if (( status == 0 )); then
    $CNVGRIB -g12 -p40 ${IMS_FILE} ./ims.grib2
  else 
    cp ${IMS_FILE} ./ims.grib2
  fi
  $WGRIB2 ims.grib2 -match "ICEC" -grib ims.icec.grib2
  grid173="0 0 0 0 0 0 0 0 4320 2160 0 0 89958000 42000 48 -89958000 359958000 83000 83000 0"
  $COPYGB2 -x -i3 -g "$grid173" ims.icec.grib2 ims.icec.5min.grib2
else
  msg="WARNING in ${pgm}: IMS ice data missing. Can not run program."
  if test "$use_prod_util" = "true" ; then
    postmsg "$jlogfile" "$msg"
  fi
  exit 3
fi

#------------------------------------------------------------------------
# Does EMC/MMAB 5-minute ice data exist?  If not, don't run ice blend 
# program.  Copy old blended data to current directory.
#------------------------------------------------------------------------

if [ ! -f ${FIVE_MIN_ICE_FILE} ]
then
  msg="WARNING in ${pgm}: MMAB ice data missing. Can not run program."
  if test "$use_prod_util" = "true" ; then
    postmsg "$jlogfile" "$msg"
  fi
  exit 5
fi

#------------------------------------------------------------------------
# Run program to blend data.
#------------------------------------------------------------------------

if test "$use_prod_util" = "true" ; then
  . prep_step
fi

# These are input files.
export FORT17="$FIVE_MIN_ICE_MASK_FILE"
export FORT11="ims.icec.5min.grib2"
export FORT15="$FIVE_MIN_ICE_FILE"

# This is the output blended file
export FORT51="$BLENDED_ICE_FILE"

$BLENDICEEXEC >> $pgmout 2> errfile
rc=$?

#------------------------------------------------------------------------
# Did program run without error?  
#
# If no, copy old blended ice file to current directory.
#
# If yes, convert file from grib 2 to grib 1 expected by gfs as
# follows:
#    - use wgrib 2 to set corner point longitude to 0.042 degrees
#      to prevent round-off error during the cnvgrib step.
#    - use cnvgrib to convert from grib2 to grib 1.
#    - use copygb to replace bitmap with '1.57' land flag value
#      expected by global cycle program.
#------------------------------------------------------------------------

if (( rc != 0 ))
then
  msg="WARNING: ${pgm} completed abnormally."
  if test "$use_prod_util" = "true" ; then
    postmsg "$jlogfile" "$msg"
  fi
  exit $rc
else
  $WGRIB2 -set_int 3 51 42000 ${BLENDED_ICE_FILE} -grib ${BLENDED_ICE_FILE}.corner
  $CNVGRIB -g21 ${BLENDED_ICE_FILE}.corner ${BLENDED_ICE_FILE}.bitmap
  rm $BLENDED_ICE_FILE
  $COPYGB -M "#1.57" -x ${BLENDED_ICE_FILE}.bitmap $BLENDED_ICE_FILE
  msg="${pgm} completed normally."
  if test "$use_prod_util" = "true" ; then
    postmsg "$jlogfile" "$msg"
  fi
  if [ "$SENDCOM" = "YES" ] ; then
    cp $BLENDED_ICE_FILE $COMOUT
  fi
  rm -f ${BLENDED_ICE_FILE}.corner ${BLENDED_ICE_FILE}.bitmap
fi

exit 0
