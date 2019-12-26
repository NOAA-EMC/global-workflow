#!/bin/bash
###############################################################################
#                                                                             #
# This script generates the tabular bulletins for a single output points      #
# of  WAVEATCH MWW3 implemnatation. It is run as a child script by a          # 
# postprocessor.                                                              #
#                                                                             #
# Remarks :                                                                   #
# - The necessary files are retrieved by the mother script.                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory bull_$loc which is      #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
# - This script replaces ww3_spec_bull.sh and marks transition of generating  #
#   bulletins from the point output post processor instead of a separate      #
#   partitioning code                                                         #
#                                                                             #
#                                                                Jan  2011    #
# Update log                                                                  #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  # set execution trace prompt.  ${0##*/} adds the script's basename
  PS4=" \${SECONDS} ${0##*/} L\${LINENO} + "
  set -x

  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  cd $BULLDATA

  rm -rf bull_$1
  mkdir bull_$1
  err=$?
  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '************************************************************************************ '
    echo '*** FATAL ERROR : ERROR IN ww3_spec_bull (COULD NOT CREATE TEMP DIRECTORY) *** '
    echo '************************************************************************************ '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN ww3_spec_bull (Could not create temp directory)"
    exit 1
  fi

  cd bull_$1
  
  ymdh=$2
  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!     Make spectral bulletin     |'
  echo '+--------------------------------+'
  echo "   Model ID        : $WAV_MOD_TAG"
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if buoy location set

  if [ "$#" -lt '2' ]
  then
    set +x
    echo ' '
    echo '****************************************************'
    echo '*** LOCATION ID IN ww3_spec_bull.sh NOT SET ***'
    echo '****************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "Error LOCATION ID IN ww3_spec_bull.sh NOT SET"
    exit 1
  else
    buoy=$1
    grep $buoy ${DATA}/buoy_log.ww3 > tmp_list.loc
    while read line
    do
      buoy_name=`echo $line | awk '{print $2}'`
      if [ $buoy = $buoy_name ]
      then
        point=`echo $line | awk '{ print $1 }'`
        set +x
        echo "              Location ID/#   : $buoy (${point})"
        echo "   Spectral output start time : $ymdh "
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        break
      fi
    done < tmp_list.loc
    if [ -z "$point" ]
    then
      set +x
      echo '***********************************************************'
      echo '*** LOCATION ID IN ww3_spec_bull.sh NOT RECOGNIZED ***'
      echo '***********************************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "Error LOCATION ID IN ww3_spec_bull.sh NOT RECOGNIZED"
      exit 2
    fi
  fi

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$YMDH" ] || [ -z "$date" ] || [ -z "$cycle" ] || \
     [ -z "$dtbull" ] || [ -z "$EXECwave" ] || [ -z "$WAV_MOD_TAG" ] || \
     [ -z "$ymdh" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '***********************************************************'
    echo '*** EXPORTED VARIABLES IN ww3_spec_bull.sh NOT SET ***'
    echo '***********************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "Error EXPORTED VARIABLES IN ww3_spec_bull.sh NOT SET"
    exit 3
  fi

# 0.d Starting time for output

  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
  truntime="`echo $YMDH | cut -c1-8` `echo $YMDH | cut -c9-10`0000"

  set +x
  echo "   Output starts at $tstart."
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.e sync important files

#  $FSYNC ${DATA}/mod_def.${uoutpGRD}
#  $FSYNC ${DATA}/out_pnt.${uoutpGRD}
#  $FSYNC ${DATA}/ww3_spec_bull.inp.tmpl

# 0.f Links to mother directory

  ln -s ${DATA}/mod_def.${uoutpGRD} mod_def.ww3
  ln -s ${DATA}/out_pnt.${uoutpGRD} .

# --------------------------------------------------------------------------- #
# 2.  Generate spectral data file
# 2.a Input file for postprocessor

  set +x
  echo "   Generate input file for ww3_outp"
  [[ "$LOUD" = YES ]] && set -x

  sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtbull/g" \
      -e "s/POINT/$point/g" \
      -e "s/REFT/$truntime/g" \
                               ${DATA}/ww3_spec_bull.inp.tmpl > ww3_outp.inp

# 2.b Run the postprocessor

  set +x
  echo "   Executing $EXECwave/ww3_outp"
  [[ "$LOUD" = YES ]] && set -x

  $EXECwave/ww3_outp
  err=$?

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : ERROR IN ww3_outp *** '
    echo '******************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN ww3_outp."
    exit 4
  fi

  if [ -f $buoy.bull ] && [ -f $buoy.cbull ]
  then
    mv $buoy.bull  ${STA_DIR}/bull/$WAV_MOD_TAG.$buoy.bull
    mv $buoy.cbull ${STA_DIR}/cbull/$WAV_MOD_TAG.$buoy.cbull
  else
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : BULLETIN FILE NOT FOUND *** '
    echo '********************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : BULLETIN FILE NOT FOUND"
    exit 7
  fi

  for ext in bull cbull 
  do
    if [ ! -f "${STA_DIR}/${ext}/$WAV_MOD_TAG.$buoy.$ext" ]
    then
      set +x
      echo ' '
      echo'*******************************************************' 
      echo '*** FATAL ERROR : BULLETIN FILE NOT MOVED PROPERLY ***'
      echo'*******************************************************' 
      echo ' '
      echo " file $WAV_MOD_TAG.$buoy.$ext not found in ${STA_DIR}/${ext}/ directory!!"
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "FATAL ERROR : BULLETIN FILE $WAV_MOD_TAG.$buoy.$ext NOT FOUND"
      exit 8
    fi
  done

# --------------------------------------------------------------------------- #
# 3 Clean up
 
  rm -f ww3_outp.inp
  rm -f mod_def.ww3 out_pnt.ww3
  cd ..
  mv -f bull_$buoy done.bull_$buoy

  set +x
  echo ' '
  echo 'End of ww3_spec_bull.sh at'
  date

# End of ww3_spec_bull.sh----------------------------------------------- #
