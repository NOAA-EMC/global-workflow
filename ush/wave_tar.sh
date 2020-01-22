#!/bin/bash
###############################################################################
#                                                                             #
# This script tars the sectral or bulletin files into a single file and       #
# puts it into /com. This is a separate script to enable it to be run in      #
# parallel using poe. It also tars the spectral and bulletin files of the     #
# old grids that are generated for backward compatibility                     #
#                                                                             #
# Remarks :                                                                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory TAR_$type_$ID which is  #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
#                                                            March 13, 2007   #
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

  cd $DATA
  postmsg "$jlogfile" "Making TAR FILE"


  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make tar file          |'
  echo '+--------------------------------+'
  echo "   ID              : $1"
  echo "   Type            : $2"
  echo "   Number of files : $3"
  [[ "$LOUD" = YES ]] && set -x


# 0.b Check if type set

  if [ "$#" -lt '3' ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** VARIABLES IN ww3_tar.sh NOT SET ***'
    echo '********************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "TYPE IN ww3_tar.sh NOT SET"
    exit 1
  else
    ID=$1
    type=$2
    nb=$3
  fi

  filext=$type
  if [ "$type" = "ibp" ]; then filext='spec'; fi

  rm -rf TAR_${filext}_$ID 
  mkdir  TAR_${filext}_$ID
# this directory is used only for error capturing

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$cycle" ] || [ -z "$COMOUT" ] || [ -z "$WAV_MOD_TAG" ] ||  \
     [ -z "$SENDCOM" ] || [ -z "$SENDDBN" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '*****************************************************'
    echo '*** EXPORTED VARIABLES IN ww3_tar.sh NOT SET ***'
    echo '*****************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "EXPORTED VARIABLES IN ww3_tar.sh NOT SET"
    exit 2
  fi

  cd ${STA_DIR}/${type}

# --------------------------------------------------------------------------- #
# 2.  Generate tar file (spectral files are compressed)

  set +x
  echo ' '
  echo '   Making tar file ...'

  count=0
  countMAX=5
  tardone='no'

  while [ "$count" -lt "$countMAX" ] && [ "$tardone" = 'no' ]
  do
    
    [[ "$LOUD" = YES ]] && set -v
    # JY nf=`ls $ID.*.$type | wc -l | awk '{ print $1 }'`
    nf=`ls | awk '/'$ID.*.$filext'/ {a++} END {print a}'`
    if [ "$nf" = "$nb" ]
    then 
      tar -cf $ID.$cycle.${type}_tar ./$ID.*.$filext
      exit=$?
      set +v; [[ "$LOUD" = YES ]] && set -x

      if  [ "$exit" != '0' ]
      then
        set +x
        echo ' '
        echo '***************************************** '
        echo '*** FATAL ERROR : TAR CREATION FAILED *** '
        echo '***************************************** '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "FATAL ERROR : TAR CREATION FAILED"
        exit 3
      fi
      
      if [ -f "$ID.$cycle.${type}_tar" ]
      then
        tardone='yes'
      fi
    else
      set +x
      echo ' All files not found for tar. Sleeping 10 seconds and trying again ..'
      [[ "$LOUD" = YES ]] && set -x
      sleep 10
      count=`expr $count + 1`
    fi

  done

  if [ "$tardone" = 'no' ]
  then
    set +x
    echo ' '
    echo '***************************************** '
    echo '*** FATAL ERROR : TAR CREATION FAILED *** '
    echo '***************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : TAR CREATION FAILED"
    exit 3
  fi

  if [ "$filext" = 'spec' ]
  then
    if [ -s $ID.$cycle.${type}_tar ]
    then
      file_name=$ID.$cycle.${type}_tar.gz
      /usr/bin/gzip -c $ID.$cycle.${type}_tar > ${file_name}
      exit=$?

      if  [ "$exit" != '0' ]
      then
        set +x
        echo ' '
        echo '***************************************************** '
        echo '*** FATAL ERROR : SPECTRAL TAR COMPRESSION FAILED *** '
        echo '***************************************************** '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "FATAL ERROR : SPECTRAL TAR COMPRESSION FAILED"
        exit 4
      fi
    fi
  else
    file_name=$ID.$cycle.${type}_tar
  fi

# --------------------------------------------------------------------------- #
# 3.  Move data to /com

  set +x
  echo ' '
  echo "   Moving tar file ${file_name} to $COMOUT ..."
  [[ "$LOUD" = YES ]] && set -x

  cp ${file_name} $COMOUT/station/.

  exit=$?

  if  [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '************************************* '
    echo '*** FATAL ERROR : TAR COPY FAILED *** '
    echo '************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : TAR COPY FAILED"
    exit 4
  fi

  if [ "$SENDDBN" = 'YES' ]
  then
    set +x
    echo ' '
    echo "   Alerting TAR file as $COMOUT/station/${file_name}"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    $DBNROOT/bin/dbn_alert MODEL OMBWAVE $job $COMOUT/station/${file_name}
  fi

# --------------------------------------------------------------------------- #
# 4.  Final clean up

  cd $DATA

  set +x; [[ "$LOUD" = YES ]] && set -v
  rm -f  ${STA_DIR}/${type}
  set +v

  echo ' '
  echo 'End of ww3_tar.sh at'
  date

# End of ww3_tar.sh ----------------------------------------------------- #
