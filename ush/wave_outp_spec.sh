#!/bin/bash
#                                                                       
################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_outp_spec.sh
# Script description:  Generates ASCII data files with the wave spectral data
#
# Author:   Hendrik Tolman      Org: NCEP/EMC      Date: 2007-03-17
# Abstract: Creates grib2 files from WW3 binary output
#
# Script history log:
# 2019-11-02  J-Henrique Alves Ported to global-workflow.
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (BASH) shell
#   Machine: WCOSS-DELL-P3
#
################################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  # set execution trace prompt.  ${0##*/} adds the script's basename
  PS4=" \${SECONDS} ${0##*/} L\${LINENO} + "
  set -x

  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x
   
  bloc=$1
  ymdh=$2
  specdir=$3

  YMDHE=`$NDATE $FHMAX_WAV $CDATE`

  cd $SPECDATA

  rm -rf ${specdir}_${bloc}
  mkdir ${specdir}_${bloc}
  err=$?
  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '****************************************************************************** '
    echo '*** FATAL ERROR : ERROR IN ww3_outp_spec (COULD NOT CREATE TEMP DIRECTORY) *** '
    echo '****************************************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN ww3_outp_spec (Could not create temp directory)"
    exit 1
  fi

  cd ${specdir}_${bloc}

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!       Make spectral file       |'
  echo '+--------------------------------+'
  echo "   Model ID        : $WAV_MOD_TAG"
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if buoy location set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '***********************************************'
    echo '*** LOCATION ID IN ww3_outp_spec.sh NOT SET ***'
    echo '***********************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "LOCATION ID IN ww3_outp_spec.sh NOT SET"
    exit 1
  else
    buoy=$bloc
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
      echo '******************************************************'
      echo '*** LOCATION ID IN ww3_outp_spec.sh NOT RECOGNIZED ***'
      echo '******************************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "LOCATION ID IN ww3_outp_spec.sh NOT RECOGNIZED"
      exit 2
    fi
  fi


# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$CDATE" ] || [ -z "$dtspec" ] || [ -z "$EXECcode" ] || \
     [ -z "$WAV_MOD_TAG" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '******************************************************'
    echo '*** EXPORTED VARIABLES IN ww3_outp_spec.sh NOT SET ***'
    echo '******************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "EXPORTED VARIABLES IN ww3_outp_spec.sh NOT SET"
    exit 3
  fi

# 0.d Starting time for output

  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
  YMD="`echo $ymdh | cut -c1-8`"
  HMS="`echo $ymdh | cut -c9-10`0000"
  set +x
  echo "   Output starts at $tstart."
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.e sync important files

#  $FSYNC ${DATA}/mod_def.${waveuoutpGRD}
#  $FSYNC ${DATA}/out_pnt.${waveuoutpGRD}
#  $FSYNC ${DATA}/ww3_outp_spec.inp.tmpl

# 0.f Links to mother directory

  ln -s ${DATA}/mod_def.${waveuoutpGRD} ./mod_def.ww3
  ln -s ${DATA}/output_${ymdh}0000/out_pnt.${waveuoutpGRD} ./out_pnt.ww3

# --------------------------------------------------------------------------- #
# 2.  Generate spectral data file
# 2.a Input file for postprocessor

  set +x
  echo "   Generate input file for ww3_outp."
  [[ "$LOUD" = YES ]] && set -x

  if [ "$specdir" = "bull" ]
  then
    tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
    truntime="`echo $CDATE | cut -c1-8` `echo $YMDH | cut -c9-10`0000"
    sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtspec/g" \
      -e "s/POINT/$point/g" \
      -e "s/REFT/$truntime/g" \
                               ${DATA}/ww3_outp_bull.inp.tmpl > ww3_outp.inp
    outfile=${buoy}.bull
    coutfile=${buoy}.cbull
  else
    sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtspec/g" \
      -e "s/POINT/$point/g" \
      -e "s/ITYPE/1/g" \
      -e "s/FORMAT/F/g" \
                               ${DATA}/ww3_outp_spec.inp.tmpl > ww3_outp.inp
    outfile=ww3.`echo $tstart | cut -c3-8``echo $tstart | cut -c10-11`.spc
  fi

# 2.b Run the postprocessor

  set +x
  echo "   Executing $EXECcode/ww3_outp"
  [[ "$LOUD" = YES ]] && set -x

  $EXECcode/ww3_outp
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
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN ww3_outp"
    exit 4
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up
# 3.a Move data to directory for station ascii files

  if [ -f $outfile ]
  then
   if [ "${ymdh}" = "${CDATE}" ]
   then
     if [ "$specdir" = "bull" ]
     then
       cat $outfile | sed -e '9,$d' >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.bull
       cat $coutfile | sed -e '8,$d' >> ${STA_DIR}/c${specdir}/$WAV_MOD_TAG.$buoy.cbull
     else
       #cat $outfile | sed -e '15,$d' >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.spec
       cat $outfile >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.spec
     fi
   elif [ "${ymdh}" = "${YMDHE}" ]
   then
     if [ "$specdir" = "bull" ]
     then
       cat $outfile | sed -e '1,7d' >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.bull
       cat $coutfile | sed -e '1,6d' >> ${STA_DIR}/c${specdir}/$WAV_MOD_TAG.$buoy.cbull
     else
       cat $outfile | sed -n "/^${YMD} ${HMS}$/,\$p" >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.spec
     fi
   else
     if [ "$specdir" = "bull" ]
     then
       cat $outfile | sed -e '1,7d' | sed -e '2,$d' >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.bull
       cat $coutfile | sed -e '1,6d' | sed -e '2,$d' >> ${STA_DIR}/c${specdir}/$WAV_MOD_TAG.$buoy.cbull
     else
       cat $outfile | sed -n "/^${YMD} ${HMS}$/,\$p" >> ${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.spec
     fi
   fi
  else
    set +x
    echo ' '
    echo '***************************************************************** '
    echo '*** FATAL ERROR : OUTPUT DATA FILE FOR BOUY $bouy NOT FOUND *** '
    echo '***************************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : OUTPUT DATA FILE FOR BOUY $bouy NOT FOUND"
    exit 5
  fi

# 3.b Clean up the rest

#  rm -f ww3_outp.inp
#  rm -f mod_def.ww3 out_pnt.ww3

  cd ..
  mv -f $specdir_$buoy done.$specdir_$buoy

  set +x
  echo ' '
  echo 'End of ww3_outp_spec.sh at'
  date

# End of ww3_outp_spec.sh ---------------------------------------------------- #
