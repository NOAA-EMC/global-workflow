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

# 0.a Basic modes of operation
buoy=$1
ymdh=$2
specdir=$3
workdir=$4

# 0.b Check if buoy location set

if [[ $# -lt 1 ]]; then
  echo 'FATAL ERROR: LOCATION ID IN wave_outp_spec.sh NOT SET'
  exit 1
else
  point=$(awk "{if (\$2 == \"${buoy}\"){print \$1; exit} }" "${DATA}/buoy_log.ww3")
  if [[ -z "${point}" ]]; then
    echo 'FATAL ERROR: LOCATION ID IN ww3_outp_spec.sh NOT RECOGNIZED'
    exit 2
  else
    printf "\n              Location ID/#   : %s (%s) ${buoy} (${point})\n   Spectral output start time : %s" "${buoy}" "${point}" "${ymdh}"
  fi
fi

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

if [[ -z "${PDY+0}" || -z "${cyc+0}" || -z "${dtspec+0}" || -z "${EXECgfs+0}" || -z "${WAV_MOD_TAG+0}" || -z "${STA_DIR+0}" ]]; then
  echo 'FATAL ERROR: EXPORTED VARIABLES IN ww3_outp_spec.sh NOT SET'
  exit 3
fi

cd "${workdir}"

rm -rf "${specdir}_${buoy}"
mkdir -p "${specdir}_${buoy}"
cd "${specdir}_${buoy}"

cat << EOF

+--------------------------------+
!       Make spectral file       |
+--------------------------------+
   Model ID        : ${WAV_MOD_TAG}
EOF

# 0.d sync important files

#  $FSYNC ${DATA}/mod_def.${waveuoutpGRD}
#  $FSYNC ${DATA}/out_pnt.${waveuoutpGRD}
#  $FSYNC ${DATA}/ww3_outp_spec.inp.tmpl

# 0.e Links to mother directory

${NLN} "${DATA}/output_${ymdh}0000/mod_def.${waveuoutpGRD}" ./mod_def.ww3
${NLN} "${DATA}/output_${ymdh}0000/out_pnt.${waveuoutpGRD}" ./out_pnt.ww3

# --------------------------------------------------------------------------- #
# 2.  Generate spectral data file
# 2.a Input file for postprocessor

echo "   Generate input file for ww3_outp."

tstart="${ymdh:0:8} ${ymdh:8:2}0000"
printf "   Output starts at %s.\n" "${tstart}"

if [[ "${specdir}" == "bull" ]]; then
  truntime="${PDY} ${cyc}0000"
  sed -e "s/TIME/${tstart}/g" \
    -e "s/DT/${dtspec}/g" \
    -e "s/POINT/${point}/g" \
    -e "s/REFT/${truntime}/g" \
    "${DATA}/ww3_outp_bull.inp.tmpl" > ww3_outp.inp
  outfile="${buoy}.bull"
  coutfile="${buoy}.cbull"
else
  sed -e "s/TIME/${tstart}/g" \
    -e "s/DT/${dtspec}/g" \
    -e "s/POINT/${point}/g" \
    -e "s/ITYPE/1/g" \
    -e "s/FORMAT/F/g" \
    "${DATA}/ww3_outp_spec.inp.tmpl" > ww3_outp.inp
  outfile="ww3.${tstart:2:5}${tstart:9:2}.spc"
fi

# 2.b Run the postprocessor

export pgm="${NET,,}_ww3_outp.x"
source prep_step

echo "   Executing ${EXECgfs}/${pgm}"

"${EXECgfs}/${pgm}" 1> "outp_${specdir}_${buoy}.out" 2>&1
export err=$?
if [[ ${err} -ne 0 ]]; then
  echo "FATAL ERROR : ERROR IN ${pgm} *** "
  exit 4
fi

# --------------------------------------------------------------------------- #
# 3.  Clean up
# 3.a Move data to directory for station ascii files

YMDHE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} + ${FHMAX_WAV_PNT} hours")
model_start_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} + ${OFFSET_START_HOUR} hours")

if [[ -f "${outfile}" ]]; then
 if [[ "${ymdh}" == "${model_start_date}" ]]; then
   if [[ "${specdir}" == "bull" ]]; then
     sed '9,$d' "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.bull"
     sed '8,$d' "${coutfile}" >> "${STA_DIR}/c${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.cbull"
   else
     cat "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.spec"
   fi
 elif [[ "${ymdh}" == "${YMDHE}" ]]; then
   if [[ "${specdir}" == "bull" ]]; then
     sed '1,7d' "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.bull"
     sed '1,6d' "${coutfile}" >> "${STA_DIR}/c${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.cbull"
   else
     sed -n "/^${ymdh:0:8} ${ymdh:8:2}0000$/,\$p" "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.spec"
   fi
 else
   if [[ "${specdir}" == "bull" ]]; then
     sed '8q;d' "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.bull"
     sed '7q;d' "${coutfile}" >> "${STA_DIR}/c${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.cbull"
   else
     sed -n "/^${ymdh:0:8} ${ymdh:8:2}0000$/,\$p" "${outfile}" >> "${STA_DIR}/${specdir}fhr/${WAV_MOD_TAG}.${ymdh}.${buoy}.spec"
   fi
 fi
else
  echo "FATAL ERROR: OUTPUT DATA FILE FOR BUOY '${buoy}' NOT FOUND"
  exit 5
fi

# 3.b Clean up the rest

cd "${workdir}" || exit 1
rm -rf "${specdir}_${buoy}"

# End of ww3_outp_spec.sh ---------------------------------------------------- #
