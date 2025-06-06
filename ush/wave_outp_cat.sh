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

# 0.a Basic modes of operation
  bloc=$1
  MAXHOUR=$2
  specdir=$3

# 0.b Check if buoy location set

  if [[ $# -lt 1 ]]
  then
    echo 'FATAL ERROR: LOCATION ID IN ww3_outp_spec.sh NOT SET ***'
    exit 1
  else
    buoy=${bloc}
  fi

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [[ -z "${DTPNT_WAV+0}" || -z "${FHMIN_WAV+0}" || -z "${WAV_MOD_TAG+0}" || -z "${STA_DIR+0}" ]]
  then
    echo 'FATAL ERROR: EXPORTED VARIABLES IN ww3_outp_cat.sh NOT SET ***'
    exit 3
  fi


# --------------------------------------------------------------------------- #
# 1. Cat for a buoy all fhr into one file

  echo "   Generate input file for ww3_outp."

  if [[ "${specdir}" == "bull" ]]
  then
    outfile=${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.bull
    coutfile=${STA_DIR}/c${specdir}/$WAV_MOD_TAG.$buoy.cbull
    rm -f "${outfile}" "${coutfile}"
  else
    outfile=${STA_DIR}/${specdir}/$WAV_MOD_TAG.$buoy.spec
    rm -f "${outfile}"
  fi

  fhr=$FHMIN_WAV
  fhrp=$fhr
  while [[ ${fhr} -le ${MAXHOUR} ]]; do

    ymdh=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} + ${fhr} hours")
    if [[ "$specdir" == "bull" ]]
    then
      outfilefhr=${STA_DIR}/${specdir}fhr/$WAV_MOD_TAG.${ymdh}.$buoy.bull
      coutfilefhr=${STA_DIR}/c${specdir}fhr/$WAV_MOD_TAG.${ymdh}.$buoy.cbull
    else
      outfilefhr=${STA_DIR}/${specdir}fhr/$WAV_MOD_TAG.${ymdh}.$buoy.spec
    fi

    if [[ -f "${outfilefhr}" ]]
    then
      if [[ "$specdir" == "bull" ]]
      then
        cat "${outfilefhr}" >> "${STA_DIR}/${specdir}/${WAV_MOD_TAG}.${buoy}.bull"
        cat "${coutfilefhr}" >> "${STA_DIR}/c${specdir}/${WAV_MOD_TAG}.${buoy}.cbull"
        rm -f "${outfilefhr}" "${coutfilefhr}"
      else
        cat "${outfilefhr}" >> "${STA_DIR}/${specdir}/${WAV_MOD_TAG}.${buoy}.spec"
        #rm -f "${outfilefhr}"
      fi
    else
      echo "FATAL ERROR: OUTPUT DATA FILE FOR BUOY ${buoy} at ${ymdh} NOT FOUND"
      exit 9
    fi

    FHINCP=$(( DTPNT_WAV / 3600 ))
    if [[ ${fhr} -eq ${fhrp} ]]
    then
      fhrp=$((fhr+FHINCP))
    fi
    echo "${fhrp}"

    fhr=${fhrp} # no gridded output, loop with out_pnt stride

  done

  if [[ ! -f "${outfile}" ]]
  then
    echo "FATAL ERROR: OUTPUTFILE ${outfile} not created    "
    exit 2
  fi

# End of ww3_outp_cat.sh ---------------------------------------------------- #
