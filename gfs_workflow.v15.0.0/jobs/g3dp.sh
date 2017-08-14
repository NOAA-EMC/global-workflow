#!/bin/ksh
################################################################################
# This script downscaling g3df* to lower resolution, g3dc*.
#
# Usage: g3dp.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   FHINI
#   FHGOC3D
#   FHOUT
#   GRID_IDD
#   in_o
#   DATATMP
#   SHDIR
#   PBEG
#   PEER
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

set -a;. $CONFIG;set +a
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 0;mkdir -p $DATA||exit 0;cd $DATA||exit 0
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-..}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################# Set other variables

export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export COPYGB=${COPYGB:-$NWPROD/util/exec/copygb}
##
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
nknd=1

export FHINI=$(eval echo \${FHINIFCST$cycle$cdump:-0}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-3}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-0}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00}|cut -f$nknd -d,)  
export FHGOC3D=${FHGOC3D:-72}
##
#
export GRID_IDD=${GRID_IDD:-3}
export IDRT=${IDRT:-0}
export in_o=${in_o:-3}  ## default is budget from fine to coarse


export VERBOSE=${VERBOSE:-YES}
export G3DIND=${G3DIND:-$COMROT}
export COMOUT=$COMROT
datafcst=$G3DIND
#

export COPYGBOPTS="-K-1 -B-1 -g$GRID_IDD -i$in_o -x"

export SUFOUT=.${CDUMP}.$CDATE

#
################################################################################
# 
# Post process g3d files in mpmd mode

cmdfile=$DATA/cmdfile
if [ -s $cmdfile ] ; then rm $cmdfile ; fi
> $cmdfile
FH=-$FHOUT
until [[ $((FH=10#$FH+10#$FHOUT)) -gt $FHGOC3D ]];do [[ $FH -lt 10 ]]&&FH=0$FH
  export G3DINP=$datafcst/g3df$FH.$CDUMP.$CDATE
  export G3DOUT=$COMOUT/g3dc${FH}${SUFOUT}
  if [ ${RM_G3DOUT:-NO} = YES ] ; then /bin/rm $G3DOUT ; fi
  [ ! -s $G3DINP ] && exit 1
  #  Post GRIB - resolution reduction
  if [ ! -s $G3DOUT ] ; then
    echo $COPYGB $COPYGBOPTS $G3DINP $G3DOUT >>$cmdfile
  fi
done
if [ -s $cmdfile ] ; then
  cat $cmdfile
  ntasks=$(echo $LOADL_PROCESSOR_LIST|wc -w)
  # only valid if count .le. 128
  [ $ntasks -eq 128 ] && ntasks=$(poe hostname|wc -l)
  remainder=$(($ntasks-$(cat $cmdfile|wc -l)%$ntasks))
  n=0;while [ $((n+=1)) -le $remainder ] ;do
      echo "echo do nothing" >>$cmdfile
  done
  l=0
  n=-1
  while read line ;do ((n+=1))
      if [ $((n%ntasks)) -eq 0 ] ; then
          ((l+=1))
          >cmdlist.$l
      fi
      echo "$line" >> cmdlist.$l
  done < $cmdfile
  n=0
  while [ $((n+=1)) -le $l ] ;do
      poe -pgmmodel mpmd -cmdfile cmdlist.$n
  done
fi

rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 3;fi

  ###  make sure g3d file is there before deleting the original file
if [ ${RM_ORIG_G3D:-NO} = YES ] ; then
  FH=-$FHOUT
  until [[ $((FH=10#$FH+10#$FHOUT)) -gt $FHGOC3D ]];do [[ $FH -lt 10 ]]&&FH=0$FH
    export G3DINP=$datafcst/g3df$FH.$CDUMP.$CDATE
    export G3DOUT=$COMOUT/g3dc${FH}${SUFOUT}
    if [[ ! -s $G3DOUT ]] ; then
      echo " cannot find $G3DOUT"
      exit 8
    else
    ### remove higher resolution dataset
      ls -l $G3DOUT > outx
      cat outx
      cnt=`cat outx|awk '{print $5}'`
##Sarah temp fix##   if [ $cnt -gt 28000000 ] ; then
      if [ $cnt -gt 20000000 ] ; then
          echo "$G3DOUT has count of $cnt; ok to clean up"
          rm $G3DINP
      fi
    fi
  done
fi
   
################################################################################
# Exit gracefully
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 4;fi
$PEND
