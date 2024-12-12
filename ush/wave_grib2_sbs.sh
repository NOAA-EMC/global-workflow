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

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

cd "${GRIBDATA}" || exit 2

alertName=${RUN^^}

grdID=$1
gribDIR="${grdID}_grib"
rm -rfd "${gribDIR}"
mkdir "${gribDIR}"
err=$?
case ${grdID} in    
    glo_10m) GRDNAME='global.0p16';;
    glo_15mxt) GRDNAME='global.0p25';;       
    glo_30mxt) GRDNAME='global.0p50';;
    glo_30m) GRDNAME='global.0p50';;
    glo_025) GRDNAME='global.0p25';;
    glo_100) GRDNAME='global.1p00';;
    glo_200) GRDNAME='global.2p00';;
    glo_500) GRDNAME='global.5p00';;
    at_10m) GRDNAME='atlocn.0p16';;
    ep_10m) GRDNAME='epacif.0p16';;
    wc_10m) GRDNAME='wcoast.0p16';;
    ak_10m) GRDNAME='alaska.0p16';;
    aoc_9km) GRDNAME='arctic.9km';;
    ant_9km) GRDNAME='antarc.9km';;
    gnh_10m) GRDNAME='global.0p16';;
    gsh_15m) GRDNAME='gsouth.0p25';;
    ao_20m) GRDNAME='arctic.0p33';;
    so_20m) GRDNAME='antarc.0p33';;
    reg025) GRDNAME='global.0p25';;
    gwes_30m) GRDNAME='global.0p50';;
    *)
    echo "FATAL ERROR: No grid specific wave config values exist for ${grdID}. Aborting."
    exit 1;;
esac
if [[ ${err} != 0 ]]; then
  set +x
  echo ' '
  echo '******************************************************************************* '
  echo '*** FATAL ERROR : ERROR IN ww3_grib2 (COULD NOT CREATE TEMP DIRECTORY) *** '
  echo '******************************************************************************* '
  echo ' '
  set_trace
  exit 1
fi

cd "${gribDIR}" || exit 2

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
FH3=$(printf %03i "${fhr}")

# Verify if grib2 file exists from interrupted run
ENSTAG=""
if [[ -n ${waveMEMB} ]]; then ENSTAG=".${membTAG}${waveMEMB}" ; fi
outfile="${WAV_MOD_TAG}.${cycle}${ENSTAG}.${grdnam}.${grdres}.f${FH3}.grib2"

# Only create file if not present in COM
if [[ ! -s "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}.idx" ]]; then

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make GRIB files        |'
  echo '+--------------------------------+'
  echo "   Model ID         : $WAV_MOD_TAG"
  set_trace

  if [[ -z "${PDY}" ]] || [[ -z ${cyc} ]] || [[ -z "${cycle}" ]] || [[ -z "${EXECgfs}" ]] || \
	 [[ -z "${COMOUT_WAVE_GRID}" ]] || [[ -z "${WAV_MOD_TAG}" ]] || [[ -z "${gribflags}" ]] || \
	 [[ -z "${GRIDNR}" ]] || [[ -z "${MODNR}" ]] || \
     [[ -z "${SENDDBN}" ]]; then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** EXPORTED VARIABLES IN postprocessor NOT SET ***'
    echo '***************************************************'
    echo ' '
    set_trace
    exit 1
  fi

  # 0.c Starting time for output

  tstart="${ymdh:0:8} ${ymdh:8:2}0000"

  set +x
  echo "   Starting time    : ${tstart}"
  echo "   Time step        : Single SBS"
  echo "   Number of times  : Single SBS"
  echo "   GRIB field flags : ${gribflags}"
  echo ' '
  set_trace

  # 0.e Links to working directory

  ${NLN} "${DATA}/mod_def.${grdID}" "mod_def.ww3"
  ${NLN} "${DATA}/output_${ymdh}0000/out_grd.${grdID}" "out_grd.ww3"

  # --------------------------------------------------------------------------- #
  # 1.  Generate GRIB file with all data
  # 1.a Generate input file for ww3_grib2
  #     Template copied in mother script ...

  set +x
  echo "   Generate input file for ww3_grib2"
  set_trace

  sed -e "s/TIME/${tstart}/g" \
      -e "s/DT/${dtgrib}/g" \
      -e "s/NT/${ngrib}/g" \
      -e "s/GRIDNR/${GRIDNR}/g" \
      -e "s/MODNR/${MODNR}/g" \
      -e "s/FLAGS/${gribflags}/g" \
      "${DATA}/ww3_grib2.${grdID}.inp.tmpl" > ww3_grib.inp


  echo "ww3_grib.inp"
  cat ww3_grib.inp

  # 1.b Run GRIB packing program

  set +x
  echo "   Run ww3_grib2"
  echo "   Executing ${EXECgfs}/ww3_grib"
  set_trace

  export pgm=ww3_grib;. prep_step
  "${EXECgfs}/ww3_grib" > "grib2_${grdnam}_${FH3}.out" 2>&1
  export err=$?;err_chk

  if [ ! -s gribfile ]; then
    set +x
    echo ' '
    echo '************************************************ '
    echo '*** FATAL ERROR : ERROR IN ww3_grib encoding *** '
    echo '************************************************ '
    echo ' '
    set_trace
    exit 3
  fi

  if (( fhr > 0 )); then 
    ${WGRIB2} gribfile -set_date "${PDY}${cyc}" -set_ftime "${fhr} hour fcst" -grib "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}"
    err=$?
  else
    ${WGRIB2} gribfile -set_date "${PDY}${cyc}" -set_ftime "${fhr} hour fcst" \
      -set table_1.4 1 -set table_1.2 1 -grib "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}"
    err=$?
  fi

  if [[ ${err} != 0 ]]; then
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : ERROR IN ww3_grib2 *** '
    echo '********************************************* '
    echo ' '
    set_trace
    exit 3
  fi

  # Create index
  ${WGRIB2} -s "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}" > "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}.idx"

  # Create grib2 subgrid is this is the source grid
  if [[ "${grdID}" = "${WAV_SUBGRBSRC}" ]]; then
    for subgrb in ${WAV_SUBGRB}; do
      subgrbref=$(echo ${!subgrb} | cut -d " " -f 1-20)
      subgrbnam=$(echo ${!subgrb} | cut -d " " -f 21)
      subgrbres=$(echo ${!subgrb} | cut -d " " -f 22)
      subfnam="${WAV_MOD_TAG}.${cycle}${ENSTAG}.${subgrbnam}.${subgrbres}.f${FH3}.grib2"
      ${COPYGB2} -g "${subgrbref}" -i0 -x "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}" "${COMOUT_WAVE_GRID}/${GRDNAME}/${subfnam}"
      ${WGRIB2} -s "${COMOUT_WAVE_GRID}/${GRDNAME}/${subfnam}" > "${COMOUT_WAVE_GRID}/${GRDNAME}/${subfnam}.idx"
   done
  fi

  # 1.e Save in /com

  if [[ ! -s "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}" ]]; then
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : ERROR IN ww3_grib2 *** '
    echo '********************************************* '
    echo ' '
    echo " Error in moving grib file ${outfile} to com"
    echo ' '
    set_trace
    exit 4
  fi
  if [[ ! -s "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}.idx" ]]; then
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** FATAL ERROR : ERROR IN ww3_grib2 INDEX FILE *** '
    echo '*************************************************** '
    echo ' '
    echo " Error in moving grib file ${outfile}.idx to com"
    echo ' '
    set_trace
    exit 4
  fi

  if [[ "${SENDDBN}" = 'YES' ]] && [[ ${outfile} != *global.0p50* ]]; then
    set +x
    echo "   Alerting GRIB file as ${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}"
    echo "   Alerting GRIB index file as ${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}.idx"
    set_trace
    "${DBNROOT}/bin/dbn_alert" MODEL "${alertName}_WAVE_GB2" "${job}" "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${alertName}_WAVE_GB2_WIDX" "${job}" "${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile}.idx"
  else
    echo "${outfile} is global.0p50 or SENDDBN is NO, no alert sent"
  fi


  # --------------------------------------------------------------------------- #
  # 3.  Clean up the directory

  rm -f gribfile

  set +x
  echo "   Removing work directory after success."
  set_trace

  cd ../
  mv -f "${gribDIR}" "done.${gribDIR}"

else
  set +x
  echo ' '
  echo " File ${COMOUT_WAVE_GRID}/${GRDNAME}/${outfile} found, skipping generation process"
  echo ' '
  set_trace
fi


# End of ww3_grib2.sh -------------------------------------------------- #
