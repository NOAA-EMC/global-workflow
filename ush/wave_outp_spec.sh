#! /usr/bin/env bash

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
# 2020-06-10  J-Henrique Alves Ported to R&D machine Hera
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
  ymdh=$2
  specdir=$3
  workdir=$4

  YMDHE=$($NDATE $FHMAX_WAV_PNT $CDATE)
  model_start_date=$(${NDATE} ${OFFSET_START_HOUR} "${PDY}${cyc}")

  cd $workdir

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
    set_trace
    exit 1
  fi

  cd ${specdir}_${bloc}

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!       Make spectral file       |'
  echo '+--------------------------------+'
  echo "   Model ID        : $WAV_MOD_TAG"
  set_trace

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
    point=$(awk "{if (\$2 == \"${buoy}\"){print \$1; exit} }" "${DATA}/buoy_log.ww3")
    if [ -z "$point" ]
    then
      set +x
      echo '******************************************************'
      echo '*** LOCATION ID IN ww3_outp_spec.sh NOT RECOGNIZED ***'
      echo '******************************************************'
      echo ' '
      set_trace
      exit 2
    else
      set +x
      echo "              Location ID/#   : $buoy (${point})"
      echo "   Spectral output start time : $ymdh "
      echo ' '
    fi
  fi


# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$CDATE" ] || [ -z "$dtspec" ] || [ -z "${EXECgfs}" ] || \
     [ -z "$WAV_MOD_TAG" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '******************************************************'
    echo '*** EXPORTED VARIABLES IN ww3_outp_spec.sh NOT SET ***'
    echo '******************************************************'
    echo ' '
    set_trace
    exit 3
  fi

# 0.d Starting time for output

  tstart="$(echo $ymdh | cut -c1-8) $(echo $ymdh | cut -c9-10)0000"
  YMD="$(echo $ymdh | cut -c1-8)"
  HMS="$(echo $ymdh | cut -c9-10)0000"
  set +x
  echo "   Output starts at $tstart."
  echo ' '
  set_trace

# 0.e sync important files

#  $FSYNC ${DATA}/mod_def.${waveuoutpGRD}
#  $FSYNC ${DATA}/out_pnt.${waveuoutpGRD}
#  $FSYNC ${DATA}/ww3_outp_spec.inp.tmpl

# 0.f Links to mother directory

  ${NLN} ${DATA}/output_${ymdh}0000/mod_def.${waveuoutpGRD} ./mod_def.ww3
  ${NLN} ${DATA}/output_${ymdh}0000/out_pnt.${waveuoutpGRD} ./out_pnt.ww3

# --------------------------------------------------------------------------- #
# 2.  Generate spectral data file
# 2.a Input file for postprocessor

  set +x
  echo "   Generate input file for ww3_outp."
  set_trace

  if [ "$specdir" = "bull" ]
  then
    tstart="$(echo $ymdh | cut -c1-8) $(echo $ymdh | cut -c9-10)0000"
    truntime="$(echo $CDATE | cut -c1-8) $(echo $CDATE | cut -c9-10)0000"
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
    outfile=ww3.$(echo $tstart | cut -c3-8)$(echo $tstart | cut -c10-11).spc
  fi

# 2.b Run the postprocessor

  export pgm="${NET,,}_ww3_outp.x"
  source prep_step

  set +x
  echo "   Executing ${EXECgfs}/${pgm}"
  set_trace

  "${EXECgfs}/${pgm}" 1> outp_${specdir}_${buoy}.out 2>&1
  export err=$?;err_chk
  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : ERROR IN ${pgm} *** '
    echo '******************************************** '
    echo ' '
    set_trace
    exit 4
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up
# 3.a Move data to directory for station ascii files

  if [ -f $outfile ]
  then
   if [ "${ymdh}" = "${model_start_date}" ]
   then
     if [ "$specdir" = "bull" ]
     then
       sed '9,$d' "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.bull"
       sed '8,$d' "${coutfile}" >> "${STA_DIR}/c${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.cbull"
     else
       cat $outfile >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.spec"
     fi
   elif [ "${ymdh}" = "${YMDHE}" ]
   then
     if [ "$specdir" = "bull" ]
     then
       sed '1,7d' "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.bull"
       sed '1,6d' "${coutfile}" >> "${STA_DIR}/c${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.cbull"
     else
       sed -n "/^${YMD} ${HMS}$/,\$p" "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.spec"
     fi
   else
     if [ "$specdir" = "bull" ]
     then
       sed '8q;d' "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.bull"
       sed '7q;d' "${coutfile}" >> "${STA_DIR}/c${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.cbull"
     else
       sed -n "/^${YMD} ${HMS}$/,\$p" "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.spec"
     fi
   fi
  else
    set +x
    echo ' '
    echo '***************************************************************** '
    echo "*** FATAL ERROR : OUTPUT DATA FILE FOR BOUY ${bouy} NOT FOUND *** "
    echo '***************************************************************** '
    echo ' '
    set_trace
    exit 5
  fi

# 3.b Clean up the rest

cd ..
rm -rf "${specdir}_${bloc}"

# End of ww3_outp_spec.sh ---------------------------------------------------- #
