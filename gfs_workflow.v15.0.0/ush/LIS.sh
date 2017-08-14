#!/bin/ksh
if [ $# -lt  3 ] ; then
  echo "USAGE: $0 start_date end_date grid [CDUMP] [COMROT] [rundir]"
exit 8
fi
set -x
#
export BASEDIR=${BASEDIR:-/global/save/wx23sm/GFS/f2010/trunk/para}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export FIXDIR=${FIXDIR:-$BASEDIR/fix}
export FIX_LM=${FIX_LM:-$BASEDIR/fix/fix_lm}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export OBERRFLAG=${OBERRFLAG:-.false.}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export NHOUR=${NHOUR:-$NWPROD/util/exec/nhour}
export cfsd=${cfsd:-cfs_cdas_}
export LISEXEC=${LISEXEC:-$BASEDIR/exec/${cfsd}LIS}
export GLDASCYCHR=${GLDASCYCHR:-24}
typeset -L1 l=$PGMOUT
[[ $l = '&' ]]&&a=''||a='>'
export REDOUT=${REDOUT:-'1>'$a}
typeset -L1 l=$PGMERR
[[ $l = '&' ]]&&a=''||a='>'
export REDERR=${REDERR:-'2>'$a}
#
### ARGUMENTS

start_date=$1
end_date=$2
grid=$3
if [[ $grid -ne 126 && $grid -ne 382 && $grid -ne 574 ]]; then exit; fi
CDUMP=${4:-${CDUMP:-gdas}}
COMROT=${5:-$COMROT}
rundir=${6:-$DATA}
RESDIR=${RESDIR:-$COMROT/RESTART}
#
mkdir -p $rundir
mkdir -p $rundir/input
#
#   Imported variables
#
CMAPDIR=${CMAPDIR:-$COMROT}
#CMAPDIR=${CMAPDIR:-/climate/noscrub/wx22mj/LIS/GDAS_para/input/CMAP}
FIX_LIS=${FIX_LIS:-$FIX_LM/FIX_T$grid}
script=$USHDIR
ln -s $COMROT $rundir/input/GDAS
#mkdir -p $rundir/input/CMAP
#ln -s $CMAPDIR $rundir/input/CMAP

#${NCP:-/bin/cp} $COMDMP
#
export MP_SHARED_MEMORY=yes
export MP_STDOUTMODE=ordered
export MP_LABELIO=yes
#

### LOG

date1=$(echo $start_date | cut -c1-8)
date2=$(echo $end_date | cut -c1-8)
log="LIS.T${grid}.log.$date1.$date2.log"
rm -f $log
touch $log
echo $script  >> $log

### CHECK CMAP UPDATE

prev_date=$($NDATE -$GLDASCYCHR $CDATE)
if [ -s $COMROT/cmap.date ] ; then
 CMAPdate1=`cat $COMROT/cmap.date`
 nn=$(echo $CMAPdate1 | wc -c)
 if [ $nn -gt 1 ] ; then
   if [ $CMAPdate1 -gt $CDATE ] ; then
    CMAPdate1=$prev_date
   else
    yyyymmdd=$(echo $CMAPdate1 | cut -c1-8)
    prev_cyc=$(echo $prev_date | cut -c9-10)
    CMAPdate1=${yyyymmdd}${prev_cyc}
   fi
 else
  CMAPdate1=$prev_date
 fi
else
 CMAPdate1=$prev_date
fi
#
currdir=$(pwd)
cd $CMAPDIR
cyc=$(echo $CMAPdate1 | cut -c9-10)
CMAPfile=$(ls -1 cmap.${CDUMP}*${cyc} | tail -1)
CMAPdate2=$(basename $CMAPfile | cut -c 11-20)
nn=$(echo $CMAPdate2 | wc -c)
if [ $nn -gt 1 ] ; then
 if [ $CMAPdate2 -gt $CDATE ] ; then CMAPdate2=$CDATE ; fi
 echo "CMAP UPDATES FROM $CMAPdate1 TO $CMAPdate2"   >> $log

### DETERMINE CURRENT EXECUTION TIME

 if [[ $CMAPdate1 -ne $CMAPdate2 && $CMAPdate1 -lt ${date1}00 ]] then
  start_date=$CMAPdate1
  date1=$(echo $start_date | cut -c1-8)
 fi
else
 echo "NO CMAP PRECIP files available - fcst precip used" >> $log
fi

cd $currdir

echo "LIS  RUNS    FROM $date1 TO $date2"  >> $log

### CARDFILE

$script/lis.crd.sh $date1 $date2 $grid $rundir

#cp ${script}lis.crd $rundir/lis.crd

### FIX FIELDS

rm -fr ${LIS}FIX
ln -s  $FIX_LIS $rundir/FIX

### INITIAL LAND STATES

yyyy=`echo $date1 | cut -c1-4`

rm -f $rundir/noah.rst

#
 rst_date=$start_date

echo $COMROT/noah.rst.$CDUMP.$rst_date >> $log
${NCP:-/bin/cp} $COMROT/noah.rst.$CDUMP.$rst_date $rundir/noah.rst

### Make the LIS RUN

date=$($NDATE -24 $start_date)
gdas_int=$((24/gdas_cyc))
while [[ $date -lt $end_date ]]; do
 yymmdd=`echo $date | cut -c1-8`
 cyc=`echo $date | cut -c9-10`
 for fhr in 00 01 02 03 04 05 06 ; do
  mkdir -p $rundir/input/GDAS/$CDUMP.$yymmdd
  ln -fs $COMROT/flxf$fhr.$CDUMP.${yymmdd}${cyc} $rundir/input/GDAS/$CDUMP.$yymmdd/gdas1.t${cyc}z.sfluxgrbf$fhr 
 done
 date=$($NDATE $gdas_int $date)
done
#
date=$start_date
while [[ $date -le $end_date ]]; do
 yymmdd=`echo $date | cut -c1-8`
 cyc=`echo $date | cut -c9-10`
 ln -fs $CMAPDIR/cmap.$CDUMP.${yymmdd}${cyc} $rundir/cmap.$CDUMP.${yymmdd}${cyc}
 date=$($NDATE $gdas_int $date)
done
#

PGM=$LISEXEC
eval $PGM $REDOUT$PGMOUT $REDERR$PGMERR

### REPORT

if [ $(echo $CMAPdate2 | wc -c) -gt 1 ] ; then
 rm -f ./tmp
 echo  $CMAPdate2 > ./tmp
 mv ./tmp $COMROT/cmap.date
fi

rm -f ./tmp
echo $date1 $date2 $grid > ./tmp
mv ./tmp $COMROT/gldas2gfs.T${grid}.date

yyyymmdd=$(echo $end_date | cut -c1-8)
yyyy=$(echo $end_date | cut -c1-4)
cd $rundir/EXP$grid/NOAH/$yyyy/$yyyymmdd
${NCP:-/bin/cp} LIS.E$grid.${yyyymmdd}*.NOAH.grb $COMROT
${NCP:-/bin/cp} LIS.E$grid.${yyyymmdd}*.NOAHgbin $COMROT
${NCP:-/bin/cp} LIS.E$grid.${end_date}.Noahrst $COMROT/noah.rst.$CDUMP.$end_date
#
date=$($NDATE -24 $end_date)
until [[ $date -lt $start_date ]] ; do
 yyyymmdd=$(echo $date | cut -c1-8)
 yyyy=$(echo $date | cut -c1-4)
 cd $rundir/EXP$grid/NOAH/$yyyy/$yyyymmdd
 ${NCP:-/bin/cp} LIS.E$grid.${yyyymmdd}*.N* $COMROT
 date=$($NDATE -24 $date)
done
