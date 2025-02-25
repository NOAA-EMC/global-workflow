#! /usr/bin/env bash

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
# 2019-11-02  J-Henrique Alves Ported to global-workflow.
#                                                                             #
# Update log                                                                  #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

  cd $DATA

  rm -rf ice
  mkdir ice
  cd ice
  ${NLN} "${DATA}/postmsg" postmsg

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make ice fields        |'
  echo '+--------------------------------+'
  echo "   Model TAG       : $WAV_MOD_TAG"
  echo "   Model ID        : ${RUN}wave"
  echo "   Ice grid ID     : $WAVEICE_FID"
  echo "   Ice file        : $WAVICEFILE"
  echo ' '
  set_trace
  echo "Making ice fields."

  if [[ -z "${YMDH}" ]] || [[ -z "${cycle}" ]] || \
     [[ -z "${COMOUT_WAVE_PREP}" ]] || [[ -z "${FIXgfs}" ]] || [[ -z "${EXECgfs}" ]] || \
     [[ -z "${WAV_MOD_TAG}" ]] || [[ -z "${WAVEICE_FID}" ]] || [[ -z "${COMIN_OBS}" ]]; then

    set +x
    echo ' '
    echo '**************************************************'
    echo '*** EXPORTED VARIABLES IN preprocessor NOT SET ***'
    echo '**************************************************'
    echo ' '
    exit 1
    set_trace
    echo "NON-FATAL ERROR - EXPORTED VARIABLES IN preprocessor NOT SET"
  fi

# 0.c Links to working directory

  ${NLN} ${DATA}/mod_def.$WAVEICE_FID mod_def.ww3

# --------------------------------------------------------------------------- #
# 1.  Get the necessary files
# 1.a Copy the ice data file

  file=${COMIN_OBS}/${WAVICEFILE}

  if [ -f $file ]
  then
    cp $file ice.grib
  fi

  if [ -f ice.grib ]
  then
    set +x
    echo "   ice.grib copied ($file)."
    set_trace
  else
    set +x
    echo ' '
    echo '************************************** '
    echo "*** FATAL ERROR: NO ICE FILE $file ***  "
    echo '************************************** '
    echo ' '
    set_trace
    echo "FATAL ERROR - NO ICE FILE (GFS GRIB)"
    exit 2
  fi

# --------------------------------------------------------------------------- #
# 2.  Process the GRIB packed ice file
# 2.a Unpack data

  set +x
  echo '   Extracting data from ice.grib ...'
  set_trace

  $WGRIB2 ice.grib -netcdf icean_5m.nc 2>&1 > wgrib.out


  err=$?

  if [ "$err" != '0' ]
  then
    cat wgrib.out
    set +x
    echo ' '
    echo '**************************************** '
    echo '*** ERROR IN UNPACKING GRIB ICE FILE *** '
    echo '**************************************** '
    echo ' '
    set_trace
    echo "ERROR IN UNPACKING GRIB ICE FILE."
    exit 3
  fi

  rm -f wgrib.out
  rm -f ice.grib
  rm -f ice.index


# 2.d Run through preprocessor wave_prep

  set +x
  echo '   Run through preprocessor ...'
  echo ' '
  set_trace

  cp -f ${DATA}/ww3_prnc.ice.$WAVEICE_FID.inp.tmpl ww3_prnc.inp

  export pgm="${NET,,}_ww3_prnc.x"
  source prep_step

  "${EXECgfs}/${pgm}" 1> prnc_${WAVEICE_FID}_${cycle}.out 2>&1
  export err=$?; err_chk
  if [ "$err" != '0' ]
  then
    cat prnc_${WAVEICE_FID}_${cycle}.out
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** WARNING: NON-FATAL ERROR IN ww3_prnc *** '
    echo '******************************************** '
    echo ' '
    set_trace
    echo "WARNING: NON-FATAL ERROR IN ww3_prnc."
    exit 4
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
    icefile=${RUN}wave.${WAVEICE_FID}.$cycle.ice
  fi

  set +x
  echo "   Saving ice.ww3 as ${COMOUT_WAVE_PREP}/${icefile}"
  set_trace
  cp ice.ww3 "${COMOUT_WAVE_PREP}/${icefile}"
  rm -f ice.ww3

# --------------------------------------------------------------------------- #
# 4.  Clean up the directory

cd ..

rm -rf ice

# End of waveice.sh --------------------------------------------------------- #
