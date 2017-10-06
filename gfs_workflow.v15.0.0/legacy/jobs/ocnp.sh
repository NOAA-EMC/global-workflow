#!/bin/ksh 
################################################################################
# This script runs the post processor.
# Usage: ocnp.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
################################################################################
set -ux

################################################################################
# Go configure

set -a;. $CONFIG;set +a
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
eval export DATA=$TMPDIR
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-..}
export EXECDIR=${EXECDIR:-$BASEDIR/exec}
export FIXDIR=${FIXDIR:-$BASEDIR/fix/fix_am}
export FIXgsm=${FIXgsm:-$FIXDIR}
export SCRDIR=${SCRDIR:-$BASEDIR/scripts}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables
POE_OPTS=${POE_OPTS:-"-pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered"}
export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-cp}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export NHOUR=${NHOUR:-$NWPROD/util/exec/nhour}
export cfsp=${cfsp:-}
export cfsd=${cfsd:-}
export COPYGB=${COPYGB:-$NWPROD/util/exec/copygb}
#export MOM4POSTSH=${MOM4POSTSH:-$USHDIR/mom4_post.sh}
export aveHrlyNc=${aveHrlyNc:-$BASEDIR/exec/${cfsd}aveHrlyNc}
export aveHrlyIceNc=${aveHrlyIceNc:-$BASEDIR/exec/${cfsd}aveHrlyIceNc}
export APRUN=${APRUN:-""}
export APRNOCNP=${APRNOCNP:-$APRUN}
#
#  To compute meridional overturning circulation
export mfcstcpl=${mfcstcpl:-0}
export mkmoc=${mkmoc:-1}
#
export nreg=${nreg:-5}
export tripolat=${tripolat:-65.0}
export OCN2GRIBEXEC=${OCN2GRIBEXEC:-$BASEDIR/exec/${cfsp}tripo2reg}
#
export OCNDIR=${OCNDIR:-$COMROT}
export COMOUT=${COMOUT:-$COMROT}

#
export AVG_FCST=${AVG_FCST:-NO}
export AVG_SUB=${AVG_SUB:-YES}
export AVGSUBSH=${AVGSUBSH:-$SHDIR/pavg}
export TSER_FCST=${TSER_FCST:-NO}
# AVG_INT is used for both averaging and time-series extraction
if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then 
  export AVG_INT=${AVG_INT:-999} 
else
  export AVG_INT="-1"  # default added for variable to be defined in here file ocn_post.sh
fi
export FCST_TIMEMEANSH=${FCST_TIMEMEANSH:-$BASEDIR/ush/fcst_timemean.sh}
export OCNMEANDIR=${OCNMEANDIR:-$COMOUT}
export INDXDIR=${INDXDIR:-$DATA/index}
#    For extracting time series of selected variables
export TIMEDIR=${TIMEDIR:-$COMROT}
export TSER_SUB=${TSER_SUB:-YES}
export TSERSUBSH=${TSERSUBSH:-$SHDIR/ptsr}
export FCST_TSERSH=${FCST_TSERSH:-$BASEDIR/ush/fcst_timeser.sh}

export EXTERNAL_AVG=${EXTERNAL_AVG:-NO}

export LINKPOSTFILESH=${LINKPOSTFILESH:-""}

export CCPOST=${CCPOST:-NO}
export CDFNL=${CDFNL:-fnl}
export GDUMP=${GDUMP:-$CDFNL}
export cycle=$(echo $CDATE|cut -c9-10)
export cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')

export nknd=${CKND:-1}
export FHINI=$(eval echo \${FHINIFCST$cycle$cdump:-0}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-9}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-1}|cut -f$nknd -d,)
export INCH=$(eval echo \${INCHFCST$cycle$cdump:-$((FHMAX-FHINI))}|cut -f$nknd -d,)
export GRID_ID25=$(eval echo \${GRID25FCST$cycle$cdump:-0,}|cut -f$nknd -d,)
export GRID_ID25=0 ##jsw 
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00}|cut -f$nknd -d,)
if [ $FHBAK -eq 0 ] ; then export FHINI=$FHBAK ; fi
export FH_STRT_POST=${FH_STRT_POST:-99999}
export FH_END_POST=${FH_END_POST:-99999}
export REDO_AVG=${REDO_AVG:-NO}
export JUST_TSER=${JUST_TSER:-NO}
export REDO_TSER=${REDO_TSER:-NO}

export REDO_POST=${REDO_POST:-NO}
export JUST_POST=${JUST_POST:-NO}
export in_o=${in_o:-0}       # interpolation option, defaults to 0 (bilinear)
export JUST_AVG=${JUST_AVG:-NO}
export ENS_NUM=${ENS_NUM:-1}
export omres=$(eval echo \${OMRESFCST$cycle$cdump:-05}|cut -f$nknd -d,)
export omres=${omres:-1x1}
#outres=${outres:-1x1}
export outres=${omres:-1x1}
export IGEN_ANL=${IGEN_ANL:-82}
export IGEN_FCST=${IGEN_FCST:-82}
export VERBOSE=YES
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFOUT=.${CDUMP}$nknd.$CDATE
else
 export SUFOUT=.$CDUMP.$CDATE
fi
#
if [ $outres = '05' ] ; then
 export im_ocn=720
 export jm_ocn=360
 export flatn=89.75
 export flats=-89.75
 export flonw=0.25
 export flone=359.75
fi
export km_mom4=${km_mom4:-40}
if [ $omres = '05' ] ; then
 export im_mom4=720
 export jm_mom4=410
 export jmtp_mom4=50
 export imos=160
elif [ $omres = '1x1' ] ; then
 export im_mom4=360
 export jm_mom4=231
 export jmtp_mom4=25
 export imos=80
fi

export year=$(echo $CDATE | cut -c1-4)
export month=$(echo $CDATE | cut -c5-6)
export day=$(echo $CDATE | cut -c7-8)
export hour=$(echo $CDATE | cut -c9-10)
export idbug=${idbug:-1}
export im=${im_mom4:-360}
export jm=${jm_mom4:-231}
export km=${km_mom4:-40}
export jmtp=${jmtp_mom4:-25}
export imos=${imos:-80}
export imo=${im_ocn:-360}
export jmo=${jm_ocn:-180}
export jmtpo=${jmtpo:-$((jmtp*imo/im))}
export flatn=${flatn:-89.5}
export flats=${flats:--89.5}
export flonw=${flonw:-0.5}
export flone=${flone:-359.5}

# Defaults to 1x1 output


export imo=${im_ocn:-360}
if [ $imo -gt 360 ] ; then
 export SUFO=h
else
 export SUFO=f
fi

#
#                    To start post from the middle
if [ $FH_STRT_POST -ne 99999 ] ; then
 FHINI=$FH_STRT_POST
elif [ -s $COMROT/FHREST.$CDUMP.$CDATE.$nknd ] ; then
 read FHINI < $COMROT/FHREST.$CDUMP.$CDATE.$nknd
fi
if [ $FH_END_POST -ne 99999 ] ; then
 FHEND=$FH_END_POST
else
 FHEND=$FHMAX
fi
HRMAX=$((FHEND-FHINI))
if [ $INCH -gt $HRMAX ] ; then INCH=$HRMAX ; fi
if [ $FHINI -gt $FHEND ] ; then
 echo ' FHINI > FHEND Post processing stopped, FHINI= '$FHINI', FHEND= '$FHEND
fi
#
#
#
if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then
  export xdate=$($NDATE $FHINI $CDATE)
  if [ $AVG_INT -eq 999 ] ; then              # Monthly mean
    export ydate=$(echo $xdate | cut -c1-6)0100
    if [ $ydate -gt $CDATE ] ; then
      export PDATE=$ydate
    else
      export PDATE=$CDATE
    fi
  else
    export ydate=$CDATE
    until  [[ $ydate -gt $xdate ]] ; do
      export PDATE=$ydate
      export ydate=$($NDATE $AVG_INT $PDATE)
    done
  fi
else
   PDATE=$CDATE
fi
#
#
#

export sdate=$($NDATE $FHINI $CDATE)
export edate=$($NDATE $FHEND $CDATE)
#
#
if [ $cdump = GDAS -a ${AVRG_HOURLY:-YES} = YES ] ; then
 date_av=$($NDATE 6 $sdate)
 set -A ocnfiles 6
 set -A icefiles 6
fi

if [[ $CCPOST = YES ]];then
 export tsleep=20
 export msleep=480
fi

# herefile to make tripo namelists

cat<<\EOF>ocn_post.mpi
set -e; echo $*; numlist=$1; shift 1
if [ $# -lt 4 ] ; then echo "Usage:$0  date bdate FH DO_POST [hh_inc_ocn] [TM] [mkmoc]";exit 1;fi
date=$1
bdate=$2
FH=$3
DO_POST=$4
export hh_inc_ocn=${5:-$FHOUT}
TM=${6:-""}
export mkmoc=${7:-0}

cd $DATA

if [ $DO_POST = YES ] ; then
  #    Post ocean files from netcdf to grib
  #
  #export FIX_OM=${FIX_OM:-$BASEDIR/fix/fix_om/}
  #${NCP:-/bin/cp} -p $FIX_OM/OCNINTP${omres}TO${outres}.C  OCNINTPCOEF.C
  #${NCP:-/bin/cp} -p $FIX_OM/OCNINTP${omres}TO${outres}.T  OCNINTPCOEF.T
  ymd=$(echo $date | cut -c1-8)
  yyyy=$(echo $date | cut -c1-4)
  mm=$(echo $date | cut -c5-6)
  dd=$(echo $date | cut -c7-8)
  hh=$(echo $date | cut -c9-10)


  export ocnfile=$OCNDIR/ocn_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
  if [ ! -s $ocnfile ] ; then $PERR;exit 1;fi
  export icefile=$OCNDIR/ice_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
  if [ ! -s $icefile ] ; then $PERR;exit 1;fi
  export outfile=$COMOUT/ocn${SUFO}${FH}${TM}$SUFOUT
  if [ $mkmoc -eq 1 ] ; then
    export mocfile=$COMOUT/moc${SUFO}${FH}${TM}$SUFOUT
  fi
  output=cfs_mom4_daily_proc.${ymd}_${hh}$TM.out
  error=cfs_mom4_daily_proc.${ymd}_${hh}$TM.err
  export fh=$FH

  if [ $numlist -lt 10 ]; then 
    numlist=00$numlist
  elif [ $numlist -lt 100 ]; then 
    numlist=0$numlist
  fi

cat<<eof>>ocn_post_naml.$numlist.$bdate
&triponl  
 idbug=$idbug
 mkmoc=$mkmoc
 mfcstcpl=$mfcstcpl
 igenocnp=$IGEN_OCNP
 icefile="$icefile"
 ocnfile="$ocnfile"
 outfile="$outfile"
 mocfile="$mocfile"
 iyr=$year
 imth=$month
 iday=$day  
 ihr=$hour
 mfh=$fh
 mfhout=$hh_inc_ocn
 im=$im  
 jm=$jm 
 imo=$imo
 jmo=$jmo
 jmtp=$jmtp
 jmtpo=$jmtpo
 imos=$imos
 km=$km
 flats=$flats   
 flatn=$flatn  
 flonw=$flonw    
 flone=$flone   
 nreg=$nreg
 tripolat=$tripolat
 outfilf="$COMOUT/ocnf${FH}${TM}$SUFOUT"
 outfill="$COMOUT/ocnl${FH}${TM}$SUFOUT"
 grid25=$GRID_ID25
/
eof
fi
EOF

chmod +x ocn_post.mpi

export bdate=$($NDATE $FHOUT $sdate)
export incdate=$edate

until [[ $bdate -gt $edate ]] ; do

 if [ $CCPOST = YES ] ; then
  export incdate=$($NDATE $((INCH-$FHOUT)) $bdate)
 fi

 date=$bdate
 FH=$($NHOUR $date $CDATE)
 FH=$((FH+0))
 cmd=cmd.$$
 if [ -s $cmd ] ; then rm $cmd ; fi ; >$cmd; chmod 755 $cmd


 nh=0
 until [[ $date -gt $incdate ]] ; do
   if [ $FH -lt 10 ] ; then FH=0$FH ; fi
   ymd=`echo $date | cut -c1-8`
   yyyy=`echo $date | cut -c1-4`
   mm=`echo $date | cut -c5-6`
   dd=`echo $date | cut -c7-8`
   hh=`echo $date | cut -c9-10`
#
   export ocnfile=$OCNDIR/ocn_${yyyy}_${mm}_${dd}_${hh}$SUFOUT.nc
   export icefile=$OCNDIR/ice_${yyyy}_${mm}_${dd}_${hh}$SUFOUT.nc
#  export outfile=$COMOUT/ocn${SUFO}${FH}$SUFOUT
## export outfile=$DATA/ocn${SUFO}${FH}$SUFOUT
   if [ ! -s $COMOUT/ocn${SUFO}${FH}$SUFOUT -o $REDO_POST = YES -a $JUST_AVG = NO ] ; then
     export DO_POST=YES
     if [[ $CCPOST = YES ]];then
       nsleep=0
       until [[ -s $ocnfile || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done
       if [[ $nsleep -gt $msleep ]];then $PERR;exit 2;fi
     else
	DO_POST=NO
     fi
##   output=cfs_mom4_daily_proc.${ymd}_${hh}.out
##   error=cfs_mom4_daily_proc.${ymd}_${hh}.err
     echo "$date $bdate $FH $DO_POST" >>$cmd
     if [ $cdump = GDAS -a ${AVRG_HOURLY:-YES} = YES ] ; then
       if [ $date -le $date_av -a $nknd -eq 1 ] ; then
         nh=$((nh+1))
         ocnfiles[nh]=$ocnfile
         icefiles[nh]=$icefile
         if [ $date -eq $date_av ] ; then
           export TM=${MEAN_STR:-m}
           export ocnfilem=$OCNDIR/ocn_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
           export icefilem=$OCNDIR/ice_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
           if [ -s $ocnfilem ] ; then rm $ocnfilem ; fi
           if [ -s $icefilem ] ; then rm $icefilem ; fi
           $aveHrlyNc    $ocnfilem ${ocnfiles[1]} ${ocnfiles[2]} ${ocnfiles[3]} ${ocnfiles[4]} ${ocnfiles[5]} ${ocnfiles[6]}
           $aveHrlyIceNc $icefilem ${icefiles[1]} ${icefiles[2]} ${icefiles[3]} ${icefiles[4]} ${icefiles[5]} ${icefiles[6]}
           echo "$date_av $bdate 06 $DO_POST 6 $TM $mkmoc" >>$cmd
         fi
       fi
     fi
   fi
#
   date=$($NDATE $FHOUT $date)
   FH=$((FH+FHOUT))
 done

if [ -s bdate_file ] ; then rm bdate_file  ; fi
cat<<eof>>bdate_file
&namin
bdate=$bdate
/
eof

# run the mpi code for the INCH hours
 cd $DATA

 if [ -s $cmd ] ; then
  nmpi=0; here=`pwd`;cmd=$here/$cmd; cat $cmd
  export FIX_OM=${FIX_OM:-$BASEDIR/fix/fix_om/}
 #WRK=$TMPDIR; rm -rf $WRK; mkdir -p $WRK; cd $WRK
  ${NCP:-/bin/cp} -p $FIX_OM/OCNINTP${omres}TO${outres}.C  OCNINTPCOEF.C
  ${NCP:-/bin/cp} -p $FIX_OM/OCNINTP${omres}TO${outres}.T  OCNINTPCOEF.T
   set +x
   while read line ;do
   $here/ocn_post.mpi $nmpi $line
   nmpi=`expr $nmpi + 1`
   done < $cmd
   echo; set -ux; nprc=${nprc:-12}
   #time aprun -n $nprc  $OCN2GRIBEXEC ; rc=$?
#  if [ -s bdate_file ] ; then rm bdate_file ; fi
#  echo $bdate >bdate_file
#  $APRNOCNP $OCN2GRIBEXEC < bdate_file ; rc=$?
   $APRNOCNP $OCN2GRIBEXEC ; rc=$?
 fi

 bdate=$date
done
################################################################################
# Exit gracefully
################################################################################

if [ $JUST_POST = YES -o $JUST_AVG = YES ] ; then exit ; fi
$PEND
