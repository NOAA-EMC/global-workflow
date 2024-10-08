#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_outp_cat.sh
# Script description:  Gathers ASCII data files for all fhr for each buoy
#
# Author:   Jessica Meixner      Org: NCEP/EMC      Date: 2020-08-27
# Abstract: Cats spec files from each fhr into one for each buoy
#
# Script history log:
# 2020-08-27 Jessica Meixner creation of script
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (BASH) shell
#
################################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation
  bloc=$1
  MAXHOUR=$2
  specdir=$3

# 0.b Check if buoy location set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '***********************************************'
    echo '*** LOCATION ID IN ww3_outp_spec.sh NOT SET ***'
    echo '***********************************************'
    echo ' '
    set_trace
    exit 1
  else
    buoy=$bloc
  fi

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$DTPNT_WAV" ] || [ -z "$FHMIN_WAV" ] || \
     [ -z "$WAV_MOD_TAG" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '******************************************************'
    echo '*** EXPORTED VARIABLES IN ww3_outp_cat.sh NOT SET ***'
    echo '******************************************************'
    echo ' '
    set_trace
    exit 3
  fi


# --------------------------------------------------------------------------- #
# 1. Cat for a buoy all fhr into one file

  set +x
  echo "   Generate input file for ww3_outp."
  set_trace

  if [ "$specdir" = "bull" ]
  then
    outfile=${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.bull
    coutfile=${STA_DIR}/c${specdir}/$WAV_MOD_TAG.$buoy.cbull
    for f in outfile coutfile; do
      if [[ -f ${f} ]]; then rm ${f}; fi
    done
  else
    outfile=${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.spec
    if [[ -f ${outfile} ]]; then
      rm ${outfile}
    fi    
  fi

  fhr=$FHMIN_WAV
  fhrp=$fhr
  while [ $fhr -le $MAXHOUR ]; do

    ymdh=$($NDATE $fhr $CDATE)
    if [ "$specdir" = "bull" ]
    then
      outfilefhr=${STA_DIR}/${specdir}fhr/$WAV_MOD_TAG.${ymdh}.$buoy.bull
      coutfilefhr=${STA_DIR}/c${specdir}fhr/$WAV_MOD_TAG.${ymdh}.$buoy.cbull
    else
      outfilefhr=${STA_DIR}/${specdir}fhr/$WAV_MOD_TAG.${ymdh}.$buoy.spec
    fi

    if [ -f $outfilefhr ]
    then
      if [ "$specdir" = "bull" ]
      then
        cat $outfilefhr >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.bull
        cat $coutfilefhr >> ${STA_DIR}/c${specdir}/$WAV_MOD_TAG.$buoy.cbull
        rm $outfilefhr $coutfilefhr
      else
        cat $outfilefhr >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.spec
        #rm $outfilefhr
      fi
    else
      set +x
      echo ' '
      echo '************************************************************************** '
      echo "*** FATAL ERROR : OUTPUT DATA FILE FOR BOUY $bouy at ${ymdh} NOT FOUND *** "
      echo '************************************************************************** '
      echo ' '
      set_trace
      err=2; export err;${errchk}
      exit $err
    fi

    FHINCP=$(( DTPNT_WAV / 3600 ))
    if [ $fhr = $fhrp ]
    then
      fhrp=$((fhr+FHINCP))
    fi
    echo $fhrp

    fhr=$fhrp # no gridded output, loop with out_pnt stride

  done

  if [ ! -f ${outfile} ]
  then
    set +x
    echo ' '
    echo '*************************************************** '
    echo " FATAL ERROR : OUTPUTFILE ${outfile} not created    "
    echo '*************************************************** '
    echo ' '
    set_trace
    err=2; export err;${errchk}
    exit $err
  fi

# End of ww3_outp_cat.sh ---------------------------------------------------- #
