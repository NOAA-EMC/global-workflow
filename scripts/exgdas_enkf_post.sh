#!/bin/bash
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
#   Machine: WCOSS-Cray/Theia
#
################################################################################

# Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

# Directories.
pwd=$(pwd)

# Utilities
NCP=${NCP:-"/bin/cp"}
NLN=${NLN:-"/bin/ln -sf"}

APRUN_EPOS=${APRUN_EPOS:-${APRUN:-""}}
NTHREADS_EPOS=${NTHREADS_EPOS:-1}

# Ops stuff
SENDDBN=${SENDDBN:-"NO"}

# Fix files
LEVS=${LEVS:-64}
HYBENSMOOTH=${HYBENSMOOTH:-$FIXgsi/global_hybens_smoothinfo.l${LEVS}.txt}

# Executables.
GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-$HOMEgfs/exec/getsigensmeanp_smooth.x}
GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-$HOMEgfs/exec/getsfcensmeanp.x}

# Other variables.
PREFIX=${PREFIX:-""}
SUFFIX=${SUFFIX:-""}
FHMIN=${FHMIN_EPOS:-3}
FHMAX=${FHMAX_EPOS:-9}
FHOUT=${FHOUT_EPOS:-3}
NMEM_ENKF=${NMEM_ENKF:-80}
SMOOTH_ENKF=${SMOOTH_ENKF:-"NO"}
ENKF_SPREAD=${ENKF_SPREAD:-"NO"}

################################################################################
#  Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 99

ENKF_SUFFIX="s"
[[ $SMOOTH_ENKF = "NO" ]] && ENKF_SUFFIX=""

################################################################################
# Copy executables to working directory
$NCP $GETSFCENSMEANEXEC $DATA
$NCP $GETATMENSMEANEXEC $DATA

export OMP_NUM_THREADS=$NTHREADS_EPOS

################################################################################
# Forecast ensemble member files
for imem in $(seq 1 $NMEM_ENKF); do
   memchar="mem"$(printf %03i $imem)
   for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
      fhrchar=$(printf %03i $fhr)
      $NLN $COMIN/$memchar/${PREFIX}sfcf$fhrchar${SUFFIX} sfcf${fhrchar}_$memchar
      $NLN $COMIN/$memchar/${PREFIX}atmf$fhrchar${SUFFIX} atmf${fhrchar}_$memchar
   done
done

# Forecast ensemble mean and smoothed files
for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
   fhrchar=$(printf %03i $fhr)
   $NLN $COMOUT/${PREFIX}sfcf${fhrchar}.ensmean${SUFFIX} sfcf${fhrchar}.ensmean
   $NLN $COMOUT/${PREFIX}atmf${fhrchar}.ensmean${SUFFIX} atmf${fhrchar}.ensmean
   if [ $SMOOTH_ENKF = "YES" ]; then
      for imem in $(seq 1 $NMEM_ENKF); do
         memchar="mem"$(printf %03i $imem)
         $NLN $COMOUT/$memchar/${PREFIX}atmf${fhrchar}${ENKF_SUFFIX}${SUFFIX} atmf${fhrchar}${ENKF_SUFFIX}_$memchar
      done
   fi
   [[ $ENKF_SPREAD = "YES" ]] && $NLN $COMOUT/${PREFIX}atmf${fhrchar}.ensspread${SUFFIX} atmf${fhrchar}.ensspread
done

################################################################################
# Generate ensemble mean surface and atmospheric files

[[ $SMOOTH_ENKF = "YES" ]] && $NCP $HYBENSMOOTH ./hybens_smoothinfo

rc=0
for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
   fhrchar=$(printf %03i $fhr)

   export pgm=$GETSFCENSMEANEXEC
   . prep_step

   $APRUN_EPOS ${DATA}/$(basename $GETSFCENSMEANEXEC) ./ sfcf${fhrchar}.ensmean sfcf${fhrchar} $NMEM_ENKF
   ra=$?
   ((rc+=ra))

   export_pgm=$GETATMENSMEANEXEC
   . prep_step

   if [ $ENKF_SPREAD = "YES" ]; then
      $APRUN_EPOS ${DATA}/$(basename $GETATMENSMEANEXEC) ./ atmf${fhrchar}.ensmean atmf${fhrchar} $NMEM_ENKF atmf${fhrchar}.ensspread
   else
      $APRUN_EPOS ${DATA}/$(basename $GETATMENSMEANEXEC) ./ atmf${fhrchar}.ensmean atmf${fhrchar} $NMEM_ENKF
   fi
   ra=$?
   ((rc+=ra))
done
export err=$rc; err_chk

################################################################################
# If smoothing on but no smoothing output, copy smoothed ensemble atmospheric files
if [ $SMOOTH_ENKF = "YES" ]; then
   for fhr in $(seq $FHMIN $FHOUT $FHMAX); do
      fhrchar=$(printf %03i $fhr)
      if [ ! -s atmf${fhrchar}${ENKF_SUFFIX}_mem001 ]; then
         echo WARNING! no smoothed ensemble member for fhour = $fhrchar >&2
         for imem in $(seq 1 $NMEM_ENKF); do
            memchar="mem"$(printf %03i $imem)
            $NCP atmf${fhrchar}_$memchar atmf${fhrchar}${ENKF_SUFFIX}_$memchar
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
             $DBNROOT/bin/dbn_alert MODEL GFS_ENKF $job $COMOUT/${PREFIX}sfcf${fhrchar}.ensmean${SUFFIX}
         fi
      fi
   done

fi

################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA
set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
