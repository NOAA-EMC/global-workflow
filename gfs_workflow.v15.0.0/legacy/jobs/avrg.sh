#!/bin/ksh
################################################################################
# This script runs the time averaging of analysis and forecasts.
# Usage: avrg.sh
# Imported variables:
#   CONFIG
#   CDATE
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

export PCOP=${PCOP:-$SHDIR/pcop}
export SUB=${SUB:-$SHDIR/sub}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
#
export cfsp=${cfsp:-"cfs_"}
export cfsd=${cfsd:-"cfs_cdas_"}
export TIMEAVGSH=${TIMEAVGSH:-$USHDIR/mpi_timavg_3d.sh}
export AVGPKESH=${AVGPKESH:-$USHDIR/avg_pke.sh}
export AVGPKEEXEC=${AVGPKEEXEC:-$BASEDIR/exec/${cfsd}avg_pke}
export AVRGALLSH=${AVRGALLSH:-$USHDIR/cfsrr.end_of_month.sh}
export TIMESERIESSH=${TIMESERIESSH:-$USHDIR/glbtime.end_of_month.sh}
export CFSRRPLOTSH=${CFSRRPLOTSH:-$USHDIR/cfsrr/plots/cfsrr.plots.sh}
export AVRG_ALL=${AVRG_ALL:-YES}
export RUN_PLOT_SCRIPT=${RUN_PLOT_SCRIPT:-NO}
export JUST_PLOT=${JUST_PLOT:-NO}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-gdas}
export CDAVG=${CDAVG:-$CDFNL}
export COUP_FCST=${COUP_FCST:-YES}
#export LDIAG3D=${LDIAG3D:-.false.}
export AVG_PKE=${AVG_PKE:-YES}
export TIME_SERIES=${TIME_SERIES:-NO}
export GDATE=$($NDATE -$CYINC $CDATE)
#
p=pr$(echo $PSLOT|tr '[A-Z]' '[a-z]')
#
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
export nknd=${CKND:-1}
export FHINI=$(eval echo \${FHINIFCST$cycle$cdump:-0}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-9}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-1}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00,}|cut -f$nknd -d,)
if [ $FHBAK -eq 0 ] ; then export FHINI=$FHBAK ; fi
export LDIAG3D=$(eval echo \${LD3DFCST$cycle$cdump:-$ld3d_1}|cut -f$nknd -d,)
export KO=$(eval echo \${KOPOST$cycle$cdump:-${KO:-26}}|cut -f$nknd -d,)
export anl_hr=${anl_hr:-$((24/gdas_cyc))}
export fcs_hr=${fcs_hr:-$anl_hr}
if [ $nknd -gt 1 ] ; then
 export fcs_hr=$gdas_fh
fi

myhost=$(hostname)
[[ $myhost = m* ]]&&export HOST=mist
[[ $myhost = d* ]]&&export HOST=dew

################################################################################
# Copy in information
#$PCOP $CDATE/$CDUMP/$CSTEP/DMPI $COMDMP $DATA <$RLIST
#$PCOP $CDATE/$CDUMP/$CSTEP/DMPG $COMDMPG $DATA <$RLIST


export INDIR=$COMROT
export OUTDIR=${OUTDIR_AVG:-$COMROT}
mkdir -p $OUTDIR
export RUNDIR=$DATA
export INDXDIR=$RUNDIR/index
mkdir -p $INDXDIR
#
#      Averaging for period from sdate to edate
#     (default : monthly mean of the previous month)
#
if [[ $CDUMP = $CDAVG && $CDATE = ??????0100 ]];then
 cych=$((24/gdas_cyc))
 export edate=${edate:-$($NDATE -$cych $CDATE)}
 export sdate=${sdate:-$(echo $edate | cut -c1-6)0100}
 sdate2=$($NDATE $cych $sdate)
 krec1=$(wgrib $COMROT/${FLXOSUF}f00.gdas.$sdate  | wc -l)
 krec2=$(wgrib $COMROT/${FLXOSUF}f00.gdas.$sdate2 | wc -l)
 if [ $krec2 -gt $krec1 ] ; then export sdate=$sdate2 ; fi
else
 exit
#$PEND
fi
if [ $edate -le 0 -o $sdate -ge $edate ] ; then
 echo 'NOT A VALID AVERAGING PERIOD sdate= '$sdate ' edate= '$edate
 exit
fi
#
#   Submit jobs to extract time-series of selected variables
#
#if [ $TIME_SERIES = YES ] ; then
# $TIMESERIESSH $edate $PSLOT
#fi

if [ $RUN_PLOT_SCRIPT = YES ] ; then
 if [ $nknd -eq 1 ] ; then
   $CFSRRPLOTSH $PSLOT $sdate $CDUMP $COMROT 
 fi
 if [ $JUST_PLOT = YES ] ; then exit ; fi
#if [ $JUST_PLOT = YES ] ; then $PEND ; fi
fi

#
#    If AVRG_ALL=YES, execute Suru's averaging script
#
if [ $AVRG_ALL = YES ] ; then
 if [ $nknd -eq 1 ] ; then
  $AVRGALLSH $edate $PSLOT $CDUMP $COMROT
 fi
else
#
 fhstrt=$((10#$FHINI+0))
 if [ $fhstrt -gt 0 ] ; then fhstrt=$(((10#$FHINI/10#$FHOUT+1)*10#$FHOUT)) ; fi
#
 export AVRG_FILE_NAME=${AVRG_FILE_NAME:-yyyymm}
 if [ $AVRG_FILE_NAME = yyyymm ] ; then
  yyyy=$(echo $sdate | cut -c1-4)
  mm=$(echo $sdate | cut -c5-6)
  date_str=${yyyy}${mm}
 else
  date_str=$sdate.$edate
 fi
 export date
 if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
  export SUFIN=.${CDUMP}$nknd
  export SUFOUT=.${CDUMP}$nknd.$date_str
 else
  export SUFIN=.${CDUMP}
  export SUFOUT=.${CDUMP}.$date_str
 fi

################################################################################
#
#                     Default averaging below.
#
#               Time average of analysis
#               ------------------------
 if [ $fhstrt -eq 0 -a $nknd -le 1 ] ; then
  for f in h a l ; do
   if [ -s $INDIR/pgb${f}nl$SUFIN.$sdate ] ; then
    $TIMEAVGSH fcst $sdate $edate $anl_hr pgb  ${f}nl
   fi
  done
 fi
#
if [[ $fhstrt -lt 10 ]] ; then fhstrt=0$fhstrt ; fi
#               Time average of forecasts
#               ------------------------
 for f in h f l ; do
  if [ -s $INDIR/pgb${f}${fhstrt}$SUFIN.$sdate ] ; then
#               Time average of pgb files
#               -------------------------
    $TIMEAVGSH fcst $sdate $edate $fcs_hr pgb  $f $fhstrt $FHMAX $FHOUT
  fi
#               Time average of flx files
#               -------------------------
  if [ -s $INDIR/flx${f}${fhstrt}$SUFIN.$sdate ] ; then
    $TIMEAVGSH fcst $sdate $edate $fcs_hr flx  $f $fhstrt $FHMAX $FHOUT
  fi
#               Time average of ipv files
#               -------------------------
  if [ -s $INDIR/ipv${f}${fhstrt}$SUFIN.$edate ] ; then
   $TIMEAVGSH fcst $sdate $edate $fcs_hr ipv  $f $fhstrt $FHMAX $FHOUT
  fi
#               Time average of diab files
#               -------------------------
  if [ $LDIAG3D = .true. ] ; then
   if [ -s $INDIR/diab${f}${fhstrt}$SUFIN.$edate ] ; then
    $TIMEAVGSH fcst $sdate $edate $fcs_hr diab $f $fhstrt $FHMAX $FHOUT
   fi
  fi
#               Time average of ocean files
#               ---------------------------
  if [ $COUP_FCST = YES ] ; then
   fhobeg=$((fhstrt+FHOUT))
   if [ $fhobeg -lt 10 ] ; then fhobeg=0$fhobeg ; fi
    for f in h f ; do
     if [ -s $INDIR/ocn${f}${fhobeg}$SUFIN.$edate ] ; then
      $TIMEAVGSH fcst $sdate $edate $fcs_hr ocn  $f $fhobeg $FHMAX $FHOUT
     fi
    done
   fi
 done
 if [ $AVG_PKE = YES ] ; then
          #  Some default levels
  polist_47d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
  polist_46d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,750.,700., 650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,40.,30.,20.,10.,7.,5.,4.,3.,2.,1.,0.7,0.5,0.4,0.3,0.2,0.1,0.07,0.05,0.04,0.03,0.02"
#
  polist_31d="1000.,975.,950.,925.,900.,850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
  polist_37d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
  polist_47=${polist_47:-$polist_47d}
  polist_46=${polist_46:-$polist_46d}
  polist_31=${polist_31:-$polist_31d}
  polist_37=${polist_37:-$polist_37d}
  polist=$(eval echo \${polist_$KO})
#
#               Time average pke of analysis
#               ----------------------------
  if [ $fhstrt -eq 0 -a $nknd -le 1 ] ; then
   for f in h f l ; do
    if [ -s $INDIR/pgb${f}nl$SUFIN.$sdate ] ; then
     if [ $f = h ] ; then
      export LONB=720 ; export LATB=361
     elif [ $f = f ] ; then
      export LONB=360 ; export LATB=181
     elif [ $f = l ] ; then
      export LONB=144 ; export LATB=73
     fi
     $AVGPKESH $sdate $edate $fcs_hr egy ${f}nl $LONB $LATB $KO $polist 0 0 0
    fi
   done
  fi
#               Time average pke of forecasts
#               ----------------------------
  for f in h f l ; do
   if [ -s $INDIR/pgb${f}${fhstrt}$SUFIN.$sdate ] ; then
    if [ $f = h ] ; then
      export LONB=720 ; export LATB=361
    elif [ $f = f ] ; then
      export LONB=360 ; export LATB=181
    elif [ $f = l ] ; then
      export LONB=144 ; export LATB=73
    fi
    $AVGPKESH $sdate $edate $fcs_hr egy $f $LONB $LATB $KO $polist $fhstrt $FHMAX $FHOUT
   fi
  done
 fi

fi
################################################################################
# Exit gracefully

#
rc=0
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
