#!/bin/ksh
################################################################################
# This script gets ensemble members from disk or tape
# Usage: eget.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMROT
#   COMDAY
#   PBEG
#   PERR
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

set -a;. $CONFIG;set +a  
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
export COMROTTMP=${COMROTTMP:-$COMROT}
eval export COMROT=$COMROTTMP
eval export COMDAY=${COMDAY:-$COMROT}

cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-..}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export NWPROD=${NWPROD:-$BASEDIR}
#
PBEG=${PBEG:-$SHDIR/pbeg}
PEND=${PEND:-$SHDIR/pend}
PERR=${PERR:-$SHDIR/perr}

export SMOOTH_ENKF=${SMOOTH_ENKF:-"NO"}

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-cp}
export SCP=${SCP:-/usr/bin/scp}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-gdas}
export GDATE=$($NDATE -$CYINC $CDATE)
export EGETHPSS=${EGETHPSS:-NO}

export HPSSTAR=${HPSSTAR:-$BASEDIR/ush/hpsstar}
export HTAR=${HTAR:-/apps/hpss/htar}
export HSI=${HSI:-/apps/hpss/hsi}



################################################################################
# Extract ensemble members
ENKF_SUFFIX=""
if [[ "$SMOOTH_ENKF" = "YES" ]]; then
   ENKF_SUFFIX="s"
fi
export SIGGESENS=sfg_${GDATE}_fhr06${ENKF_SUFFIX}

if [[ $EGETHPSS == YES ]]; then
   eval export efile=$EGETFILE
   $HTAR -xvf $efile ${SIGGESENS}* 
   rc=$?
   if [[ $rc -ne 0 ]]; then
      echo "EGET:  problem extracting $SIGGESENS from $efile, rc=$rc"
      $PERR
      exit 1
   fi
else
   $NCP $EGETPATH/${SIGGESENS}* ./
   rc=$?
   if [[ $rc -ne 0 ]]; then
      echo "EGET:  problem copying $SIGGESENS from EGETPATH, rc=$rc"
      $PERR
      exit 1
   fi
fi



################################################################################
# Copy ensemble members to $COMROT

imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="_mem"`printf %03i $imem`
   $NCP ${SIGGESENS}${member} $COMROT/
   rc=$?
   if [[ $rc -ne 0 ]]; then
      echo "EGET:  problem with ${SIGGESENS}${member}, rc=$rc"
      $PERR
      exit 1
   fi

   $NCP log_all ./log_old
   rm log
   echo "Process member $imem" > log
   cat log_old log > log_new
   $NCP log_new ./log_all
   
   (( imem = $imem + 1 ))
done

$NCP log_all $COMROT/fcsstat_${GDATE}_all





################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
