#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_grib2_sbs.sh
# Script description:  Create grib2 files for the wave component
#
# Author:   Hendrik Tolman      Org: NCEP/EMC      Date: 2007-07-11
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

source "$HOMEgfs/ush/preamble.sh"

# 0.a Basic modes of operation

  cd $GRIBDATA

  alertName=$(echo $RUN|tr [a-z] [A-Z])

  grdID=$1
  gribDIR=${grdID}_grib
  rm -rfd ${gribDIR}
  mkdir ${gribDIR}
  err=$?
  if [ $err != 0 ]
  then
    set +x
    echo ' '
    echo '******************************************************************************* '
    echo '*** FATAL ERROR : ERROR IN ww3_grib2 (COULD NOT CREATE TEMP DIRECTORY) *** '
    echo '******************************************************************************* '
    echo ' '
    ${TRACE_ON:-set -x}
    exit 1
  fi

  cd ${gribDIR}

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  GRIDNR=$2
  MODNR=$3
  ymdh=$4
  fhr=$5
  grdnam=$6
  grdres=$7
  gribflags=$8
  ngrib=1 # only one time slice
  dtgrib=3600 # only one time slice
# SBS one time slice per file
  FH3=$(printf %03i $fhr)

# Verify if grib2 file exists from interrupted run
  ENSTAG=""
  if [ ${waveMEMB} ]; then ENSTAG=".${membTAG}${waveMEMB}" ; fi
  outfile=${WAV_MOD_TAG}.${cycle}${ENSTAG}.${grdnam}.${grdres}.f${FH3}.grib2

# Only create file if not present in COM
  if [ ! -s ${COMOUT}/gridded/${outfile}.idx ]; then

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make GRIB files        |'
  echo '+--------------------------------+'
  echo "   Model ID         : $WAV_MOD_TAG"
  ${TRACE_ON:-set -x}

  if [ -z "$CDATE" ] || [ -z "$cycle" ] || [ -z "$EXECwave" ] || \
     [ -z "$COMOUT" ] || [ -z "$WAV_MOD_TAG" ] || [ -z "$SENDCOM" ] || \
     [ -z "$gribflags" ] || \
     [ -z "$GRIDNR" ] || [ -z "$MODNR" ] || [ -z "$SENDDBN" ]
  then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** EXPORTED VARIABLES IN postprocessor NOT SET ***'
    echo '***************************************************'
    echo ' '
    ${TRACE_ON:-set -x}
    exit 1
  fi

# 0.c Starting time for output

  tstart="$(echo $ymdh | cut -c1-8) $(echo $ymdh | cut -c9-10)0000"

  set +x
  echo "   Starting time    : $tstart"
  echo "   Time step        : Single SBS
  echo "   Number of times  : Single SBS
  echo "   GRIB field flags : $gribflags"
  echo ' '
  ${TRACE_ON:-set -x}

# 0.e Links to working directory

  ln -s ${DATA}/mod_def.$grdID mod_def.ww3
  ln -s ${DATA}/output_${ymdh}0000/out_grd.$grdID out_grd.ww3

# --------------------------------------------------------------------------- #
# 1.  Generate GRIB file with all data
# 1.a Generate input file for ww3_grib2
#     Template copied in mother script ...

  set +x
  echo "   Generate input file for ww3_grib2"
  ${TRACE_ON:-set -x}

  sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtgrib/g" \
      -e "s/NT/$ngrib/g" \
      -e "s/GRIDNR/$GRIDNR/g" \
      -e "s/MODNR/$MODNR/g" \
      -e "s/FLAGS/$gribflags/g" \
                               ${DATA}/ww3_grib2.${grdID}.inp.tmpl > ww3_grib.inp


  echo "ww3_grib.inp"
  cat ww3_grib.inp
# 1.b Run GRIB packing program

  set +x
  echo "   Run ww3_grib2"
  echo "   Executing $EXECwave/ww3_grib"
  ${TRACE_ON:-set -x}

  export pgm=ww3_grib;. prep_step
  $EXECwave/ww3_grib > grib2_${grdnam}_${FH3}.out 2>&1
  export err=$?;err_chk

    if [ ! -s gribfile ]; then
      set +x
      echo ' '
      echo '************************************************ '
      echo '*** FATAL ERROR : ERROR IN ww3_grib encoding *** '
      echo '************************************************ '
      echo ' '
      ${TRACE_ON:-set -x}
      exit 3
    fi

  if [ $fhr -gt 0 ]; then 
    $WGRIB2 gribfile -set_date $CDATE -set_ftime "$fhr hour fcst" -grib ${COMOUT}/gridded/${outfile}
    err=$?
  else
    $WGRIB2 gribfile -set_date $CDATE -set_ftime "$fhr hour fcst" -set table_1.4 1 -set table_1.2 1 -grib ${COMOUT}/gridded/${outfile}
    err=$?
  fi

  if [ $err != 0 ]
  then
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : ERROR IN ww3_grib2 *** '
    echo '********************************************* '
    echo ' '
    ${TRACE_ON:-set -x}
    exit 3
  fi

# Create index
    $WGRIB2 -s $COMOUT/gridded/${outfile} > $COMOUT/gridded/${outfile}.idx

# Create grib2 subgrid is this is the source grid
  if [ "${grdID}" = "${WAV_SUBGRBSRC}" ]; then
    for subgrb in ${WAV_SUBGRB}; do
      subgrbref=$(echo ${!subgrb} | cut -d " " -f 1-20)
      subgrbnam=$(echo ${!subgrb} | cut -d " " -f 21)
      subgrbres=$(echo ${!subgrb} | cut -d " " -f 22)
      subfnam="${WAV_MOD_TAG}.${cycle}${ENSTAG}.${subgrbnam}.${subgrbres}.f${FH3}.grib2"
      $COPYGB2 -g "${subgrbref}" -i0 -x  ${COMOUT}/gridded/${outfile} ${COMOUT}/gridded/${subfnam}
      $WGRIB2 -s $COMOUT/gridded/${subfnam} > $COMOUT/gridded/${subfnam}.idx
   done
  fi

# 1.e Save in /com

    if [ ! -s $COMOUT/gridded/${outfile} ]
    then
      set +x
      echo ' '
      echo '********************************************* '
      echo '*** FATAL ERROR : ERROR IN ww3_grib2 *** '
      echo '********************************************* '
      echo ' '
      echo " Error in moving grib file ${outfile} to com"
      echo ' '
      ${TRACE_ON:-set -x}
      exit 4
    fi
    if [ ! -s $COMOUT/gridded/${outfile} ]
    then
      set +x
      echo ' '
      echo '*************************************************** '
      echo '*** FATAL ERROR : ERROR IN ww3_grib2 INDEX FILE *** '
      echo '*************************************************** '
      echo ' '
      echo " Error in moving grib file ${outfile}.idx to com"
      echo ' '
      ${TRACE_ON:-set -x}
      exit 4
    fi

    if [[ "$SENDDBN" = 'YES' ]] && [[ ${outfile} != *global.0p50* ]]
    then
      set +x
      echo "   Alerting GRIB file as $COMOUT/gridded/${outfile}"
      echo "   Alerting GRIB index file as $COMOUT/gridded/${outfile}.idx"
      ${TRACE_ON:-set -x}
      $DBNROOT/bin/dbn_alert MODEL ${alertName}_WAVE_GB2 $job $COMOUT/gridded/${outfile}
      $DBNROOT/bin/dbn_alert MODEL ${alertName}_WAVE_GB2_WIDX $job $COMOUT/gridded/${outfile}.idx
    else
      echo "${outfile} is global.0p50, not alert out"
    fi


# --------------------------------------------------------------------------- #
# 3.  Clean up the directory

  rm -f gribfile

  set +x
  echo "   Removing work directory after success."
  ${TRACE_ON:-set -x}

  cd ../
  mv -f ${gribDIR} done.${gribDIR}

  else
    set +x
    echo ' '
    echo " File ${COMOUT}/gridded/${outfile} found, skipping generation process"
    echo ' '
    ${TRACE_ON:-set -x}
  fi


# End of ww3_grib2.sh -------------------------------------------------- #
