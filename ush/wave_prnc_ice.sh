#!/bin/sh
###############################################################################
#                                                                             #
# This script preprocesses ice fields for the ocean wave models.              #
# It is run as a child scipt by the corresponding preprocessig script.        #
#                                                                             #
# Remarks :                                                                   #
# - This script runs in the work directory designated in the mother script in #
#   which it generates its own sub-directory 'ice'.                           #
# - Because this script is not essential for the running for the wave model   #
#   (as long as it runs every now and then) the error exit codes are set to   #
#   0. The main program script will then not find the file ice.ww3 and send   #
#   a message to the wave.log file.                                           #
# - See section 0.b for variables that need to be set.                        #
#                                                                             #
#  Update record :                                                            #
#                                                                             #
# - Origination: Hendrik Tolman                                01-Mar-2007    #
#                                                                             #
# Update log                                                                  #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  cd $DATA
  seton='-xa'
  setoff='+xa'
  set $seton

  rm -rf ice
  mkdir ice
  cd ice
  ln -s ${DATA}/postmsg .

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  set $setoff
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make ice fields        |'
  echo '+--------------------------------+'
  echo "   Model TAG       : $WAV_MOD_TAG"
  echo "   Model ID        : $COMPONENTwave"
  echo "   Ice grid ID     : $WAVEICE_FID"
  echo "   Ice file        : $WAVICEFILE"
  echo ' '
  set $seton
  postmsg "$jlogfile" "Making ice fields."

  if [ -z "$YMDH" ] || [ -z "$cycle" ] || \
     [ -z "$COMOUT" ] || [ -z "$FIXwave" ] || [ -z "$EXECcode" ] || \
     [ -z "$WAV_MOD_TAG" ] || [ -z "$WAVEICE_FID" ] || [ -z "$SENDCOM" ] || \
     [ -z "$COMIN_WAV_ICE" ] || [ -z "$COMPONENTwave" ]
  then
    set $setoff
    echo ' '
    echo '**************************************************'
    echo '*** EXPORTED VARIABLES IN preprocessor NOT SET ***'
    echo '**************************************************'
    echo ' '
    exit 0
    set $seton
    postmsg "$jlogfile" "NON-FATAL ERROR - EXPORTED VARIABLES IN preprocessor NOT SET"
  fi

# 0.c Links to working directory

  ln -s ${DATA}/mod_def.$WAVEICE_FID mod_def.ww3

# --------------------------------------------------------------------------- #
# 1.  Get the necessary files
# 1.a Copy the ice data file

  file=${COMIN_WAV_ICE}/${WAVICEFILE}

  if [ -f $file ]
  then
    cp $file ice.grib
  fi

  if [ -f ice.grib ]
  then
    set $setoff
    echo "   ice.grib copied ($file)."
    set $seton
  else
    set $setoff
    echo ' '
    echo '************************************** '
    echo "*** FATAL ERROR: NO ICE FILE $file ***  "
    echo '************************************** '
    echo ' '
    set $seton
    postmsg "$jlogfile" "FATAL ERROR - NO ICE FILE (GFS GRIB)"
    exit 0
  fi

# --------------------------------------------------------------------------- #
# 2.  Process the GRIB packed ice file
# 2.a Unpack data

  set $setoff
  echo '   Extracting data from ice.grib ...'
  set $seton

  $WGRIB2 ice.grib -netcdf icean_5m.nc 2>&1 > wgrib.out


  err=$?

  if [ "$err" != '0' ]
  then
    cat wgrib.out
    set $setoff
    echo ' '
    echo '**************************************** '
    echo '*** ERROR IN UNPACKING GRIB ICE FILE *** '
    echo '**************************************** '
    echo ' '
    set $seton
    postmsg "$jlogfile" "ERROR IN UNPACKING GRIB ICE FILE."
    exit 0
  fi

  rm -f wgrib.out
  rm -f ice.grib 
  rm -f ice.index


# 2.d Run through preprocessor wave_prep

  set $setoff
  echo '   Run through preprocessor ...'
  echo ' '
  set $seton

  cp -f ${DATA}/ww3_prnc.ice.$WAVEICE_FID.inp.tmpl ww3_prnc.inp

  $EXECcode/ww3_prnc > wave_prnc.out
  err=$?

  if [ "$err" != '0' ]
  then
    cat wave_prep.out
    set $setoff
    echo ' '
    echo '************************* '
    echo '*** ERROR IN waveprep *** '
    echo '************************* '
    echo ' '
    set $seton
    postmsg "$jlogfile" "NON-FATAL ERROR IN waveprep."
    exit 0
  fi

  rm -f wave_prep.out ww3_prep.inp ice.raw mod_def.ww3

# --------------------------------------------------------------------------- #
# 3.  Save the ice file
#
# Ice file name will have ensemble member number if WW3ATMIENS=T
# and only WAV_MOD_ID if WW3ATMIENS=F
#
  if [ "${WW3ATMIENS}" = "T" ]
  then 
    icefile=${WAV_MOD_TAG}.${WAVEICE_FID}.$cycle.ice
  elif [ "${WW3ATMIENS}" = "F" ]
  then 
    icefile=${COMPONENTwave}.${WAVEICE_FID}.$cycle.ice
  fi
 
  set $setoff
  echo "   Saving ice.ww3 as $COMOUT/rundata/${icefile}"
  set $seton
  cp ice.ww3 $COMOUT/rundata/${icefile}
  rm -f ice.ww3

# --------------------------------------------------------------------------- #
# 4.  Clean up the directory

  set $setoff
  echo "   Removing work directory after success."
  set $seton

  cd ..
  rm -rf ice

  set $setoff
  echo ' '
  echo 'End of waveice.sh at'
  date

# End of waveice.sh --------------------------------------------------------- #
