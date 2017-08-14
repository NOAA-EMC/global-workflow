#!/bin/ksh
################################################################################
# This script waits for select files
# Usage: echk.sh
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
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-/nwprod}
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
export PLOG=${PLOG:-$SHDIR/plog}

$PBEG


################################################################################
# Set other variables
export DISK_GLOB=${DISK_GLOB:-/global/save}
export BASEDIR=${BASEDIR:-$DISK_GLOB/wx23sm/cfsrr}
export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-cp}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-gdas}
export CUE2RUN=${CUE2RUN:-"dev"}
export CUE2RUN1=${CUE2RUN1:-"1/R"}
export ECHKTYPE=${ECHKTYPE:-"ensstat"}
export RUNLIMECHK=${RUNLIMECHK:-25}


################################################################################
# Copy out restart and output files (disabled here)

ENSSTAT=${ENSSTAT:-$COMROT/ensstat_${GDATE}_all}
GSISTAT_HIRES=${GSISTAT_HIRES:-$COMROT/gsistat.$CDFNL.$CDATE}

rc=0


################################################################################
# Check if ensemble averaging done
if [[ $ECHKTYPE = ensstat ]]; then
  nsleep=0
  tsleep=10
  msleep=30
  until [[ -s $ENSSTAT || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done
  if [[ $nsleep -gt $msleep ]]; then
    echo "$ENSSTAT does not exist"
    export ENSSTATEX="NO"

    jn=$PSLOT$CDATE$CDUMP$CSTEP
    df=$COMDAY/$jn.dayfile

    count=`ls ${df}* | wc -l`
    if [ $count -gt 0 ] ; then
      if [ $count -gt $RUNLIMECHK ] ; then
        $PLOG "$RUNLOG" ERROR "$jn reached $RUNLIMECHK run limit"
        exit 8
      fi
      suffix=`expr $count + 1`
      df=$COMDAY/${jn}.dayfile${suffix}
    fi

    ac=$ACCOUNT
    gr=$GROUP
    us=$USER

    job=$ECHKSH

    cue2run=$CUE2RUN1
    numproc="1/1/N"
    re="1000/1"
    timelim=${TIMELIMECHK:-1:00:00}
    whenrun=+0000

    qn=$cue2run
    np=$numproc
    tl=$timelim
    wr=$whenrun

    if [ $machine = IBMP6 ]; then
      en=CONFIG,CDATE,CDUMP,CSTEP,RUN_ENVIR,ECHKTYPE
      $SUB -i -a $ac -e $en -g $gr -j $jn -o $df -p $np -q $qn -r $re -t $tl -u $us -w $wr $job
      rc=$?
    else
      APRUN=${APRUN:-""}
      APRUNC=${APRUNC:-""}
      APRUNCY=${APRUNCY:-""}
      OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
      CYCLETHREAD=${CYCLETHREAD:-1}
      config_local=$COMROT/config_${CDATE}${CDUMP}${CSTEP}
      > $config_local
      echo "export CONFIG=$CONFIG"                   >> $config_local
      echo "export CDATE=$CDATE"                     >> $config_local
      echo "export CDUMP=$CDUMP"                     >> $config_local
      echo "export CSTEP=$CSTEP"                     >> $config_local
      echo "export CKSH=$CKSH"                       >> $config_local
      echo "export CKND=$CKND"                       >> $config_local
      echo "export RUN_ENVIR=$RUN_ENVIR"             >> $config_local
      echo "export APRUN=$APRUN"                     >> $config_local
      echo "export APRUNC='$APRUNC'"                 >> $config_local
      echo "export APRUNCY='$APRUNCY'"               >> $config_local
      echo "export OMP_NUM_THREADS=$OMP_NUM_THREADS" >> $config_local
      echo "export CYCLETHREAD=$CYCLETHREAD"         >> $config_local
      echo "export ECHKTYPE=$ECHKTYPE"               >> $config_local
      cat $CONFIG                                    >> $config_local
      chmod 755 $config_local

      en=CONFIG="$config_local"

      $SUB -a $ac -e $en -j $jn -o $df -p $np -q $qn -r $re -t $tl $job
      rc=$?

    fi

    if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
    $PLOG "$RUNLOG" OK "$jn submitted"

  else
    echo "$ENSSTAT exists"
    export ENSSTATEX="YES"
    cat $ENSSTAT
  fi

fi

################################################################################
# Check if high res analysis is done
if [[ $ECHKTYPE = gsistat ]]; then
  nsleep=0
  tsleep=10
  msleep=30
  until [[ -s $GSISTAT_HIRES || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done
  if [[ $nsleep -gt $msleep ]]; then
    echo "$GSISTAT_HIRES does not exist"
    export GSISTATEX="NO"

    jn=$PSLOT$CDATE$CDUMP$CSTEP
    df=$COMDAY/$jn.dayfile

    count=`ls ${df}* | wc -l`
    if [ $count -gt 0 ] ; then
      if [ $count -gt $RUNLIMECHK ] ; then
        $PLOG "$RUNLOG" ERROR "$jn reached $RUNLIMECHK run limit"
        exit 8
      fi
      suffix=`expr $count + 1`
      df=$COMDAY/${jn}.dayfile${suffix}
    fi

    ac=$ACCOUNT
    gr=$GROUP
    us=$USER

    job=$ECHKSH

    cue2run=$CUE2RUN1
    numproc="1/1/N"
    re="1000/1"
    timelim=${TIMELIMECHK:-1:00:00}
    whenrun=+0000

    qn=$cue2run
    np=$numproc
    tl=$timelim
    wr=$whenrun

    if [ $machine = IBMP6 ]; then
      en=CONFIG,CDATE,CDUMP,CSTEP,RUN_ENVIR,ECHKTYPE
      $SUB -i -a $ac -e $en -g $gr -j $jn -o $df -p $np -q $qn -r $re -t $tl -u $us -w $wr $job
      rc=$?
    else
      APRUN=${APRUN:-""}
      APRUNC=${APRUNC:-""}
      APRUNCY=${APRUNCY:-""}
      OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
      CYCLETHREAD=${CYCLETHREAD:-1}
      config_local=$COMROT/config_${CDATE}${CDUMP}${CSTEP}
      > $config_local
      echo "export CONFIG=$CONFIG"                   >> $config_local
      echo "export CDATE=$CDATE"                     >> $config_local
      echo "export CDUMP=$CDUMP"                     >> $config_local
      echo "export CSTEP=$CSTEP"                     >> $config_local
      echo "export CKSH=$CKSH"                       >> $config_local
      echo "export CKND=$CKND"                       >> $config_local
      echo "export RUN_ENVIR=$RUN_ENVIR"             >> $config_local
      echo "export APRUN=$APRUN"                     >> $config_local
      echo "export APRUNC='$APRUNC'"                 >> $config_local
      echo "export APRUNCY='$APRUNCY'"               >> $config_local
      echo "export OMP_NUM_THREADS=$OMP_NUM_THREADS" >> $config_local
      echo "export CYCLETHREAD=$CYCLETHREAD"         >> $config_local
      echo "export ECHKTYPE=$ECHKTYPE"               >> $config_local
      cat $CONFIG                                    >> $config_local
      chmod 755 $config_local

      en=CONFIG="$config_local"

      $SUB -a $ac -e $en -j $jn -o $df -p $np -q $qn -r $re -t $tl $job
      rc=$?
    fi

    if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
    $PLOG "$RUNLOG" OK "$jn submitted"

  else
    echo "$GSISTAT_HIRES exists"
    export GSISTATEX="YES"
    ls -l $GSISTAT_HIRES
  fi

fi

################################################################################
# Copy out restart and output files



################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

$PEND
