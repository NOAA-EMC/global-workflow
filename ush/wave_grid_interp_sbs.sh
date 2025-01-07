#! /usr/bin/env bash

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
# 2020-06-10  J-Henrique Alves Ported to R&D machine Hera
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (BASH) shell
#
# Requirements:
# - wgrib2 with IPOLATES library
#
################################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

  cd $GRDIDATA

  grdID=$1
  ymdh=$2
  dt=$3
  nst=$4
  echo "Making GRID Interpolation Files for $grdID."
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
    set_trace
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
  set_trace

  if [[ -z "${PDY}" ]] || [[ -z "${cyc}" ]] || [[ -z "${cycle}" ]] || [[ -z "${EXECgfs}" ]] || \
	 [[ -z "${COMOUT_WAVE_PREP}" ]] || [[ -z "${WAV_MOD_TAG}" ]] || [[ -z "${SENDDBN}" ]] || \
	 [ -z "${waveGRD}" ]
  then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** EXPORTED VARIABLES IN postprocessor NOT SET ***'
    echo '***************************************************'
    echo ' '
    echo "${PDY}${cyc} ${cycle} ${EXECgfs} ${COMOUT_WAVE_PREP} ${WAV_MOD_TAG} ${SENDDBN} ${waveGRD}"
    set_trace
    exit 1
  fi

# 0.c Links to files

  rm -f ${DATA}/output_${ymdh}0000/out_grd.$grdID

  if [ ! -f ${DATA}/${grdID}_interp.inp.tmpl ]; then
    cp "${PARMgfs}/wave/${grdID}_interp.inp.tmpl" "${DATA}/${grdID}_interp.inp.tmpl"
  fi
  ${NLN} "${DATA}/${grdID}_interp.inp.tmpl" "${grdID}_interp.inp.tmpl"

  ${NLN} "${DATA}/output_${ymdh}0000/out_grd.${waveGRD}" "out_grd.${waveGRD}"

  for ID in ${waveGRD} ${grdID}; do
    ${NLN} "${DATA}/mod_def.${ID}" "mod_def.${ID}"
  done
  

# --------------------------------------------------------------------------- #
# 1.  Generate GRID file with all data
# 1.a Generate Input file

  time="${ymdh:0:8} ${ymdh:8:2}0000"

  sed -e "s/TIME/$time/g" \
      -e "s/DT/$dt/g" \
      -e "s/NSTEPS/$nst/g" ${grdID}_interp.inp.tmpl > ww3_gint.inp

# Check if there is an interpolation weights file available

  wht_OK='no'
  if [ ! -f ${DATA}/ww3_gint.WHTGRIDINT.bin.${grdID} ]; then
    if [ -f ${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID} ]
    then
      set +x
      echo ' '
      echo " Copying ${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID} "
      set_trace
      cp ${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID} ${DATA}
      wht_OK='yes'
    else
      set +x
      echo ' '
      echo " Not found: ${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID} "
    fi
  fi
# Check and link weights file
  if [ -f ${DATA}/ww3_gint.WHTGRIDINT.bin.${grdID} ]
  then
    ${NLN} ${DATA}/ww3_gint.WHTGRIDINT.bin.${grdID} ./WHTGRIDINT.bin
  fi

# 1.b Run interpolation code

  export pgm="${NET,,}_ww3_gint.x"
  source prep_step

  set +x
  echo "   Executing ${pgm}"
  set_trace

  "${EXECgfs}/${pgm}" 1> gint.${grdID}.out 2>&1
  export err=$?;err_chk

# Write interpolation file to main TEMP dir area if not there yet
  if [ "wht_OK" = 'no' ]  # FIXME: This is never going to evaluate to true, wht_OK is a string and needs to be ${wht_OK}.  With ${wht_OK}, the next line is trying to copy into ${FIXgfs} space.  This leads to a Permission denied error. The logic here needs to be evaluated and recoded.  #TODO
  then
    cp -f ./WHTGRIDINT.bin ${DATA}/ww3_gint.WHTGRIDINT.bin.${grdID}
    cp -f ./WHTGRIDINT.bin ${FIXgfs}/wave/ww3_gint.WHTGRIDINT.bin.${grdID}
  fi


  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '*************************************************** '
    echo "*** FATAL ERROR : ERROR IN ${pgm} interpolation * "
    echo '*************************************************** '
    echo ' '
    set_trace
    exit 3
  fi

# 1.b Clean up

  rm -f grid_interp.inp
  rm -f mod_def.*
  mv out_grd.$grdID ${DATA}/output_${ymdh}0000/out_grd.$grdID

# 1.c Save in /com

  set +x
  echo "   Saving GRID file as ${COMOUT_WAVE_PREP}/${WAV_MOD_TAG}.out_grd.${grdID}.${PDY}${cyc}"
  set_trace
  cp "${DATA}/output_${ymdh}0000/out_grd.${grdID}" "${COMOUT_WAVE_PREP}/${WAV_MOD_TAG}.out_grd.${grdID}.${PDY}${cyc}"

#    if [ "$SENDDBN" = 'YES' ]
#    then
#      set +x
#      echo "   Alerting GRID file as $COMOUT/rundata/$WAV_MOD_TAG.out_grd.$grdID.${PDY}${cyc}
#      set_trace

#
# PUT DBNET ALERT HERE ....
#

#    fi

# --------------------------------------------------------------------------- #
# 2.  Clean up the directory

  cd ../
  mv -f grint_${grdID}_${ymdh} done.grint_${grdID}_${ymdh}

# End of ww3_grid_interp.sh -------------------------------------------- #
