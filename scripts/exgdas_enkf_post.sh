#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_post.sh
# Script description:  Global ensemble forecast post processing
#
# Author:        Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script post-processes global ensemble forecast output
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

source "${USHgfs}/preamble.sh"

# Directories.
pwd=$(pwd)

APRUN_EPOS=${APRUN_EPOS:-${APRUN:-""}}
NTHREADS_EPOS=${NTHREADS_EPOS:-1}

# Ops stuff
SENDDBN=${SENDDBN:-"NO"}

# Fix files
LEVS=${LEVS:-64}
HYBENSMOOTH=${HYBENSMOOTH:-${FIXgfs}/gsi/global_hybens_smoothinfo.l${LEVS}.txt}

# Executables.
GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-${EXECgfs}/getsigensmeanp_smooth.x}
GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-${EXECgfs}/getsfcensmeanp.x}

# Other variables.
PREFIX=${PREFIX:-""}
FHMIN=${FHMIN_EPOS:-3}
FHMAX=${FHMAX_EPOS:-9}
FHOUT=${FHOUT_EPOS:-3}

if [[ "${RUN}" == "enkfgfs" ]]; then
   NMEM_ENS=${NMEM_ENS_GFS:-${NMEM_ENS:-30}}
else
   NMEM_ENS=${NMEM_ENS:-80}
fi
SMOOTH_ENKF=${SMOOTH_ENKF:-"NO"}
ENKF_SPREAD=${ENKF_SPREAD:-"NO"}

################################################################################
#  Preprocessing
ENKF_SUFFIX="s"
[[ $SMOOTH_ENKF = "NO" ]] && ENKF_SUFFIX=""

################################################################################
# Copy executables to working directory
$NCP $GETSFCENSMEANEXEC $DATA
$NCP $GETATMENSMEANEXEC $DATA

export OMP_NUM_THREADS=$NTHREADS_EPOS

################################################################################
# Forecast ensemble member files
for imem in $(seq 1 $NMEM_ENS); do
   memchar="mem"$(printf %03i "${imem}")
   MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl -x \
	   COMIN_ATMOS_HISTORY:COM_ATMOS_HISTORY_TMPL

   for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
      fhrchar=$(printf %03i $fhr)
      ${NLN} "${COMIN_ATMOS_HISTORY}/${PREFIX}sfcf${fhrchar}.nc" "sfcf${fhrchar}_${memchar}"
      ${NLN} "${COMIN_ATMOS_HISTORY}/${PREFIX}atmf${fhrchar}.nc" "atmf${fhrchar}_${memchar}"
   done
done

# Forecast ensemble mean and smoothed files
MEMDIR="ensstat" YMD=${PDY} HH=${cyc} declare_from_tmpl -rx \
       COMOUT_ATMOS_HISTORY_STAT:COM_ATMOS_HISTORY_TMPL
if [[ ! -d "${COMOUT_ATMOS_HISTORY_STAT}" ]]; then mkdir -p "${COMOUT_ATMOS_HISTORY_STAT}"; fi

for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
   fhrchar=$(printf %03i $fhr)
   ${NLN} "${COMOUT_ATMOS_HISTORY_STAT}/${PREFIX}sfcf${fhrchar}.ensmean.nc" "sfcf${fhrchar}.ensmean"
   ${NLN} "${COMOUT_ATMOS_HISTORY_STAT}/${PREFIX}atmf${fhrchar}.ensmean.nc" "atmf${fhrchar}.ensmean"
   if [ $SMOOTH_ENKF = "YES" ]; then
      for imem in $(seq 1 $NMEM_ENS); do
         memchar="mem"$(printf %03i "${imem}")
         MEMDIR="${memchar}" YMD=${PDY} HH=${cyc} declare_from_tmpl -x \
		 COMIN_ATMOS_HISTORY:COM_ATMOS_HISTORY_TMPL
         ${NLN} "${COMIN_ATMOS_HISTORY}/${PREFIX}atmf${fhrchar}${ENKF_SUFFIX}.nc" "atmf${fhrchar}${ENKF_SUFFIX}_${memchar}"
      done
   fi
   [[ $ENKF_SPREAD = "YES" ]] && ${NLN} "${COMOUT_ATMOS_HISTORY_STAT}/${PREFIX}atmf${fhrchar}.ensspread.nc" "atmf${fhrchar}.ensspread"
done

################################################################################
# Generate ensemble mean surface and atmospheric files

[[ $SMOOTH_ENKF = "YES" ]] && $NCP $HYBENSMOOTH ./hybens_smoothinfo

rc=0
for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
   fhrchar=$(printf %03i $fhr)

   export pgm=$GETSFCENSMEANEXEC
   . prep_step

   $APRUN_EPOS ${DATA}/$(basename $GETSFCENSMEANEXEC) ./ sfcf${fhrchar}.ensmean sfcf${fhrchar} $NMEM_ENS
   ra=$?
   rc=$((rc+ra))

   export_pgm=$GETATMENSMEANEXEC
   . prep_step

   if [ $ENKF_SPREAD = "YES" ]; then
      $APRUN_EPOS ${DATA}/$(basename $GETATMENSMEANEXEC) ./ atmf${fhrchar}.ensmean atmf${fhrchar} $NMEM_ENS atmf${fhrchar}.ensspread
   else
      $APRUN_EPOS ${DATA}/$(basename $GETATMENSMEANEXEC) ./ atmf${fhrchar}.ensmean atmf${fhrchar} $NMEM_ENS
   fi
   ra=$?
   rc=$((rc+ra))
done
export err=$rc; err_chk

################################################################################
# If smoothing on but no smoothing output, copy smoothed ensemble atmospheric files
if [ $SMOOTH_ENKF = "YES" ]; then
   for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
      fhrchar=$(printf %03i $fhr)
      if [ ! -s atmf${fhrchar}${ENKF_SUFFIX}_mem001 ]; then
         echo WARNING! no smoothed ensemble member for fhour = $fhrchar >&2
         for imem in $(seq 1 $NMEM_ENS); do
            memchar="mem"$(printf %03i $imem)
            ${NCP} "atmf${fhrchar}_${memchar}" "atmf${fhrchar}${ENKF_SUFFIX}_${memchar}"
         done
      fi
   done
fi

################################################################################
# Send DBN alerts
if [ $SENDDBN = "YES" ]; then

   for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
      fhrchar=$(printf %03i $fhr)
      if [ $(expr $fhr % 3) -eq 0 ]; then
         if [ -s ./sfcf${fhrchar}.ensmean ]; then
             ${DBNROOT}/bin/dbn_alert "MODEL" "GFS_ENKF" "${job}" "${COMOUT_ATMOS_HISTORY_STAT}/${PREFIX}sfcf${fhrchar}.ensmean.nc"
         fi
      fi
   done

fi

################################################################################
#  Postprocessing
cd $pwd

exit $err
