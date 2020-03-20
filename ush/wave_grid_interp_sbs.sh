#!/bin/bash
#                                                                       
################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_grid_interp_sbs.sh
# Script description:  Interpolate from native grids to target grid
#
# Author:   J-Henrique Alves    Org: NCEP/EMC      Date: 2019-11-02
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
# Requirements:                                                             
# - wgrib2 with IPOLATES library                                            
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

  cd $GRDIDATA

  grdID=$1  
  ymdh=$2
  dt=$3
  nst=$4
  postmsg "$jlogfile" "Making GRID Interpolation Files for $grdID."
  rm -rf grint_${grdID}_${ymdh}
  mkdir grint_${grdID}_${ymdh}
  err=$?

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '************************************************************************************* '
    echo '*** FATAL ERROR : ERROR IN ww3_grid_interp (COULD NOT CREATE TEMP DIRECTORY) *** '
    echo '************************************************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN ww3_grid_interp (Could not create temp directory)"
    exit 1
  fi

  cd grint_${grdID}_${ymdh}

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make GRID files        |'
  echo '+--------------------------------+'
  echo "   Model ID         : $WAV_MOD_TAG"
  [[ "$LOUD" = YES ]] && set -x

  if [ -z "$CDATE" ] || [ -z "$cycle" ] || [ -z "$EXECcode" ] || \
     [ -z "$COMOUT" ] || [ -z "$WAV_MOD_TAG" ] || [ -z "$SENDCOM" ] || \
     [ -z "$SENDDBN" ] || [ -z "$waveGRD" ]
  then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** EXPORTED VARIABLES IN postprocessor NOT SET ***'
    echo '***************************************************'
    echo ' '
    echo "$CDATE $cycle $EXECcode $COMOUT $WAV_MOD_TAG $SENDCOM $SENDDBN $waveGRD"
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "EXPORTED VARIABLES IN postprocessor NOT SET"
    exit 1
  fi

# 0.c Links to files

  rm -f ${DATA}/output_${ymdh}0000/out_grd.$grdID
  
  if [ ! -f ${DATA}/${grdID}_interp.inp.tmpl ]; then
    cp $FIXwave/${grdID}_interp.inp.tmpl ${DATA}
  fi
  ln -sf ${DATA}/${grdID}_interp.inp.tmpl . 

  for ID in $waveGRD
  do
    ln -sf ${DATA}/output_${ymdh}0000/out_grd.$ID .
  done

  for ID in $waveGRD $grdID 
  do
    ln -sf ${DATA}/mod_def.$ID .
  done

# --------------------------------------------------------------------------- #
# 1.  Generate GRID file with all data
# 1.a Generate Input file

  time="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
  YMD=$(echo $ymdh | cut -c1-8)
  HMS="$(echo $ymdh | cut -c9-10)0000"

  sed -e "s/TIME/$time/g" \
      -e "s/DT/$dt/g" \
      -e "s/NSTEPS/$nst/g" ${grdID}_interp.inp.tmpl > ww3_gint.inp

# Check if there is an interpolation weights file available

  wht_OK='no'
  if [ ! -f ${DATA}/WHTGRIDINT.bin.${grdID} ]; then
    if [ -f $FIXwave/WHTGRIDINT.bin.${grdID} ]
    then
      set +x
      echo ' '
      echo " Copying $FIXwave/WHTGRIDINT.bin.${grdID} "
      [[ "$LOUD" = YES ]] && set -x
      cp $FIXwave/WHTGRIDINT.bin.${grdID} ${DATA}
      wht_OK='yes'
    else
      set +x
      echo ' '
      echo " Not found: $FIXwave/WHTGRIDINT.bin.${grdID} "
    fi
  fi
# Check and link weights file
  if [ -f ${DATA}/WHTGRIDINT.bin.${grdID} ]
  then
    ln -s ${DATA}/WHTGRIDINT.bin.${grdID} ./WHTGRIDINT.bin
  fi

# 1.b Run interpolation code

  set +x
  echo "   Run ww3_gint
  echo "   Executing $EXECcode/ww3_gint
  [[ "$LOUD" = YES ]] && set -x

  $EXECcode/ww3_gint 1> gint.{$grdID}.out 2>&1
  err=$?

# Write interpolation file to main TEMP dir area if not there yet
  if [ "wht_OK" = 'no' ]
  then
    cp -f ./WHTGRIDINT.bin ${DATA}/WHTGRIDINT.bin.${grdID}
    cp -f ./WHTGRIDINT.bin ${FIXwave}/WHTGRIDINT.bin.${grdID}
  fi
 

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** FATAL ERROR : ERROR IN ww3_gint interpolation * '
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN ww3_gint interpolation"
    exit 3
  fi

# 1.b Clean up

  rm -f grid_interp.inp
  rm -f mod_def.*
  mv out_grd.$grdID ${DATA}/output_${ymdh}0000/out_grd.$grdID

# 1.c Save in /com

  if [ "$SENDCOM" = 'YES' ]
  then
    set +x
    echo "   Saving GRID file as $COMOUT/rundata/$WAV_MOD_TAG.out_grd.$grdID.${CDATE}"
    [[ "$LOUD" = YES ]] && set -x
    cp ${DATA}/output_${ymdh}0000/out_grd.$grdID $COMOUT/rundata/$WAV_MOD_TAG.out_grd.$grdID.${YMD}.${HMS}

#    if [ "$SENDDBN" = 'YES' ]
#    then
#      set +x
#      echo "   Alerting GRID file as $COMOUT/rundata/$WAV_MOD_TAG.out_grd.$grdID.${CDATE}
#      [[ "$LOUD" = YES ]] && set -x

#
# PUT DBNET ALERT HERE ....
#

#    fi
  fi 

# --------------------------------------------------------------------------- #
# 2.  Clean up the directory

  set +x
  echo "   Removing work directory after success."
  [[ "$LOUD" = YES ]] && set -x

  cd ../
  mv -f grint_${grdID}_${ymdh} done.grint_${grdID}_${ymdh}

  set +x
  echo ' '
  echo "End of ww3_interp.sh at"
  date

# End of ww3_grid_interp.sh -------------------------------------------- #
