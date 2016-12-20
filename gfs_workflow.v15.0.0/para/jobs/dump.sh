#!/bin/ksh
################################################################################
# This script retrieves dump files.
# Usage: dump.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   RLIST
#   DATATMP
#   COMDMP
#   DMPDIR
#   NCP
#   GETDUMPSH
#   GETGDASSH
#   PBEG
#   PCOP
#   PERR
#   PEND
#   HOMDIR
#   SWITCH
################################################################################
set -ux

################################################################################
# Check machine is production
dev=`cat /etc/dev | cut -c1`
prod=`cat /etc/prod | cut -c1`
host=`hostname | cut -c1`

################################################################################
# Go configure

set -a;. $CONFIG;set +a
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
export group_name=${group_name:-g01}
export permission=${permission:-755}
#chgrp $group_name $DATA
chmod $permission $DATA
#
export BASEDIR=${BASEDIR:-/global/save/emc.glopara/svn/gfs/trunk/para}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
#
export SWITCH=${SWITCH:-'NO'}
export TRAN=${TRAN:-'NO'}
export TRANDIR=${TRANDIR:-'/global/save/emc.glopara/svn/gfs/trunk/para/exp/dump'}
export OBSPROCSH=${OBSPROCSH:-""}

################################################################################
# Move to other machine if not prod, otherwise begin

if [[ $dev = $host && $SWITCH = 'YES' ]]; then
  if [ $host = "t" ]; then
    ssh -l $LOGNAME gyre.ncep.noaa.gov "$PSUB $CONFIG $CDATE $CDUMP $CSTEP" 
    exit
  elif [ $host = "g" ]; then
    ssh -l $LOGNAME tide.ncep.noaa.gov "$PSUB $CONFIG $CDATE $CDUMP $CSTEP" 
    exit
  fi
else

$PBEG

################################################################################
# Set other variables

COMDMPTMP=${COMDMPTMP:-$COMDMP}
COMROTTMP=${COMROTTMP:-$COMROT}
eval export COMDMP=$COMDMPTMP
eval export COMROT=$COMROTTMP
export DODUMP=${DODUMP:-YES}
export DALERT=${DALERT:-YES}
export DGGDAS=${DGGDAS:-YES}
export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export GETDUMPSH=${GETDUMPSH:-$USHDIR/global_getdump.sh}
export GETGDASSH=${GETGDASSH:-$USHDIR/global_getgdas.sh}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-gdas}
export COMOUT=$DATA
export PREOUT=''
export SUFOUT='.$CDUMP.$CDATE'
export VERBOSE=YES
export GDATE=$($NDATE -$CYINC $CDATE)
tsleep=60 	#seconds to sleep before checking dump again
msleep=360	#maximum number of times to sleep
zsleep=${zsleep:-960}	#seconds to sleep after dump alert is found
nsleep=0

################################################################################
# Wait until alert file exists

if [[ $DODUMP = YES ]];then
  if [[ $DALERT = YES ]];then
    eval alertf=$DATA/alertf$SUFOUT
    >$alertf
    $GETDUMPSH $CDATE $CDUMP alertf
    until [[ -s $alertf ]];do
       if [[ $((nsleep+=1)) -gt $msleep ]];then $PERR;exit 1;fi
       sleep $tsleep
       $GETDUMPSH $CDATE $CDUMP alertf
    done
    sleep $zsleep
#   sleep 600
  fi
  
################################################################################
# Get dump files and perhaps some analysis files
  
  $GETDUMPSH $CDATE $CDUMP
  rc=$?
  if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
  if [[ $DGGDAS = YES ]];then
    if [[ $CDUMP = $CDFNL && $CDATE = ????????00 ]];then
       $GETGDASSH $CDATE gfs $SFCISUF $SIGISUF
    fi
    if [[ $CDUMP = $CDFNL && $CDATE = ????????$CYINC ]];then
       $GETGDASSH $GDATE $CDUMP biascr satang $SFCISUF $SIGISUF
    fi
  fi
  
################################################################################
# Copy out output and restart files

  if [ ! -d "$DMPDIR/$CDATE" ]; then
    mkdir $DMPDIR/$CDATE
  fi
  if [ ! -d "$COMDMP" ]; then
    mkdir -p $COMDMP
  fi
  
# Change directory group and permissions to agree w/ NCEP restricted data policies.

# chgrp $group_name $DMPDIR/$CDATE
  chmod $permission $DMPDIR/$CDATE
# chgrp $group_name $COMDMP
  chmod $permission $COMDMP

  $PCOP $CDATE/$CDUMP/$CSTEP/DMPO $DATA $COMDMP <$RLIST
  sleep $tsleep
  $PCOP $CDATE/$CDUMP/$CSTEP/DMRO $DATA $COMDMP <$RLIST
  rc=$?

################################################################################
# Wait until alert file exists

else
  eval alertf=$COMDMP/statup$SUFOUT
  until [[ -s $alertf ]];do
     if [[ $((nsleep+=1)) -gt $msleep ]];then $PERR;exit 1;fi
     sleep $tsleep
  done
  sleep $tsleep
  rc=0
fi

################################################################################
# Transfer dump files to other machines

if [[ $TRAN = 'YES' ]]; then
#if [[ $LOGNAME = emc.glopara ]]; then
  sh $TRANDIR/archive.sh $CDATE $CDUMP > $PTMP/$LOGNAME/dump/archive.out.$$ 2>&1
fi

################################################################################
# Optional special processing
$OBSPROCSH

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND

fi
