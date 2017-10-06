#!/bin/ksh
################################################################################
# This script runs the time averaging of analysis and forecasts.
# Usage: avrg.sh
# Imported variables:
#   CONFIG
#   CDATE
#   FDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMROT
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
export DISK_GLOB=${DISK_GLOB:-/global/save}
export NCP=${NCP:-/bin/cp}
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-/nwprod}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export NWPROD=${NWPROD:-$BASEDIR}
#
PBEG=${PBEG:-$SHDIR/pbeg}
PEND=${PEND:-$SHDIR/pend}
PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

export SUB=${SUB:-/u/wx20mi/bin/sub}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export NHOUR=${NHOUR:-$NWPROD/util/exec/nhour}
export TIMEMEANSH=${TIMEMEANSH:-$USHDIR/mpi_timavg_fcst.sh}
export TIMEMEANEXEC=${TIMEMEANEXEC:-$BASEDIR/exec/mpi_timavg_3d_fcst}
export CDAVG=${CDAVG:-gdas}
export COUP_FCST=${COUP_FCST:-YES}
#
p=pr$(echo $PSLOT|tr '[A-Z]' '[a-z]')
#
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
export nknd=${CKND:-1}
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-1}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00}|cut -f$nknd -d,)
export LDIAG3D=$(eval echo \${LD3DFCST$cycle$cdump:-$ld3d_1}|cut -f$nknd -d,)


################################################################################

export INDIR=$COMROT
export OUTDIR=${OUTDIR_MEAN:-COMROT}
mkdir -p $OUTDIR
export RUNDIR=$DATA
export INDXDIR=${INDXDIR:-$RUNDIR/index}
mkdir -p $INDXDIR
#
#      Averaging for period from sdate to edate
#     (default : monthly mean of the previous month)
#
#if [[ $CDUMP = $CDAVG && $FDATE = ??????0100 ]];then
 export edate=${edate:-$FDATE}
 xdate=$(echo $($NDATE -$FHOUT $edate) | cut -c1-6)0100
 export sdate=${sdate:-$($NDATE $FHOUT $xdate)}
#else
 $PEND
#fi
if [ $edate -le 0 -o $sdate -ge $edate ] ; then
 echo 'NOT A VALID AVERAGING PERIOD sdate= '$sdate ' edate= '$edate
 exit
fi
#
fhstrt=$($NHOUR $sdate $CDATE)
if [ $fhstrt -lt 10 ] ; then fhstrt=0$fhstrt ; fi
#
export AVRG_FILE_NAME=${AVRG_FILE_NAME:-yyyymm}
if [ AVRG_FILE_NAME = yyyymm ] ; then
 yyyy=$(echo $sdate | cut -c1-4)
 mm=$(echo $sdate | cut -c5-6)
 date_str=${yyyy}${mm}
else
 date_str=$sdate.$edate
fi
export date
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFIN=.${CDUMP}$nknd
 export SUFOUT=.${CDUMP}$nknd.$date_str.$CDATE
else
 export SUFIN=.${CDUMP}
 export SUFOUT=.${CDUMP}.$date_str.$CDATE
fi

for f in h f l ; do
 if [ -s $INDIR/pgb${f}${fhstrt}.$SUFIN.$sdate ] ; then
#              Time average of pgb files
#              -------------------------
   $TIMEMEANSH $CDATE $sdate $edate $FHOUT pgb  $f
#               Time average of flx files
#               -------------------------
   $TIMEMEANSH $CDATE $sdate $edate $FHOUT flx  $f
 fi
#               Time average of ipv files
#               -------------------------
 if [ -s $INDIR/ipv${f}${fhstrt}.$SUFIN.$edate ] ; then
  $TIMEMEANSH $CDATE $sdate $edate $FHOUT ipv  $f
 fi
#               Time average of d3d files
#               -------------------------
 if [ $LDIAG3D = .true. ] ; then
  if [ -s $INDIR/d3d${f}${fhstrt}.$SUFIN.$edate ] ; then
   $TIMEMEANSH $CDATE $sdate $edate $FHOUT diab $f
  fi
 fi
#               Time average of ocean files
#               -------------------------
 if [ $COUP_FCST = YES ] ; then
  if [ -s $INDIR/ocn${f}${fhstrt}.$SUFIN.$edate ] ; then
   $TIMEMEANSH $CDATE $sdate $edate $FHOUT ocn  $f
  fi
 fi
done

################################################################################# Exit gracefully

#
rc=0
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
