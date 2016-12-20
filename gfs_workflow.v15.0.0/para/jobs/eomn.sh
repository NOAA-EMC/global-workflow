#!/bin/ksh
################################################################################
# This script submits and monitors ensemble member innovations for enkf GDAS.
# Usage: eomn.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELOMG
#   PMDLOGANAL
#   DATATMP
#   COMROT
#   NCP
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
export CSTEPIN=$CSTEP
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
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
if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
 export LLQU=${LLQU:-bjobs}
elif [ $machine = THEIA ] ; then
 export LLQU=${LLQU:-$SHDIR/llqu}
else
 export LLQU=${LLQU:-""}
fi

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}

if [ $machine = IBMP6 ] ; then
  export MP_INFOLEVEL=${INFOLEVELOMG:-2}
  export MP_PMDLOG=${PMDLOGANAL:-no}
  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  export MEMORY_AFFINITY=${MEMORY_AFFINITY:-MCM}
  export MP_LABELIO=${MP_LABELIO:-yes}
  export MP_COREFILE_FORMAT=lite

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
  export LAPI_DEBUG_ENABLE_AFFINITY=YES
# export LAPI_DEBUG_MTU_4K=YES
  export MP_FIFO_MTU=4K
  export MP_SYNC_QP=YES
  export MP_SHM_ATTACH_THRESH=500000
  export MP_EUIDEVELOP=min
  export MP_USE_BULK_XFER=yes
  export MP_BULK_MIN_MSG_SIZE=64k
  export MP_RC_MAX_QP=8192
  export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
  export LAPI_DEBUG_QP_NOTIFICATION=no
  export LAPI_DEBUG_RC_INIT_SETUP=yes
  export memory_node=${memory_node:-105000}
elif [ $machine = THEIA ] ; then
  export launcher=${launcher:-mpirun}
  export MPI_BUFS_PER_PROC=${MPI_BUFS_PER_PROC:-256}
  export MPI_BUFS_PER_HOST=${MPI_BUFS_PER_HOST:-256}
  export MPI_GROUP_MAX=${MPI_GROUP_MAX:-256}
  export MPI_MEMMAP_OFF=${MPI_MEMMAP_OFF:-1}
elif [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
  if [ $machine = WCOSS ]; then
    export launcher=${launcher:-mpirun.lsf}
    export MP_EAGER_LIMIT=${MP_EAGER_LIMIT:-65536}
    export MP_COREFILE_FORMAT=${MP_COREFILE_FORMAT:-lite}
    export MP_MPILIB=${MP_MPILIB:-mpich2}
    export MP_LABELIO=${MP_LABELIO:-yes}
    export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-yes}
    export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
    export MPICH_ALLTOALL_THROTTLE=${MPICH_ALLTOALL_THROTTLE:-0}
    export MP_COLLECTIVE_OFFLOAD=${MP_COLLECTIVE_OFFLOAD:-yes}
    export MP_SINGLE_THREAD=${MP_SINGLE_THREAD:-yes}
    export KMP_STACKSIZE=${KMP_STACKSIZE:-2048m}
  elif [ $machine = WCOSS_C ]; then
    export launcher=${launcher:-aprun}
    export KMP_AFFINITY=disabled
    export OMP_STACKSIZE=2048M
  fi
fi

export launcher=${launcher:-""}
export omplace=${omplace:-""}
export PREINP=gdas1.t$(echo $CDATE|cut -c9-10)z.
export FILESTYLE=${FILESTYLEEOMN:-C}
export VERBOSE=YES
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
cdump=$(echo $GDUMP|tr '[a-z]' '[A-Z]')

export JCAP=${JCAP_ENKF:-254}

export NTHREADS_GSI=${NTHREADS_EINV:-1}
export NTHSTACK_GSI=${NTHSTACK_EINV:-1024000000}

export SUBMIT_OMN=${SUBMIT_OMN:-YES}
export RUNLIMEOMN=${RUNLIMEOMN:-25}

rc=0


################################################################################
# Set output data

EOMNSTAT=$COMROT/omgstat_${CDATE}_all
EOMGGRP=$COMROT/omgstat_${CDATE}_grp


################################################################################
# If requested, submit ensemble innovation jobs
if [[ $SUBMIT_OMN = YES ]]; then

rm -f $EOMNSTAT
#rm -f ${EOMGGRP}*
rc=0
export nstep=$NMEM_ENKF_GRP
(( ninc = $nstep - 1 ))
export igrp=1
export nbeg=1
while [[ $nbeg -le $NMEM_ENKF ]]; do
   (( nend = $nbeg + $ninc ))
   export ENSBEG=$nbeg
   export ENSEND=$nend
   export ENSGRP=$igrp
   export CSTEP="eomn"`printf %02i $igrp`
   ac=$ACCOUNT
   gr=$GROUP
   us=$USER
   qn=$CUE2RUN

   jn=$PSLOT$CDATE$CDUMP$CSTEP
   df=$COMDAY/$jn.dayfile

   if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
    npe_node_eomg_default=16
   elif [ $machine = THEIA ] ; then
    npe_node_eomg_default=24
   elif [ $machine = IBMP6 ] ; then
    npe_node_eomg_default=32
   fi

   job=$EOMGSH

   npe_node=${npe_node_eomg:-$npe_node_eomg_default}
   pe_node=${pe_node_eomg:-$npe_node}
   prereqs="true"
   numproc=$(eval echo \${NUMPROCEOMG$cdump:-${NUMPROCEOMG:-"$(($JCAP/2+3))"}})
   nthreads=${NTHREADS_EOMG:-1}
   if [ $pe_node -eq $npe_node -a $nthreads -gt 1 ] ; then
     pe_node=$((npe_node/nthreads))
   fi
   nodes=$((numproc*nthreads/npe_node))
   if [ $numproc -gt $((nodes*npe_node/nthreads)) ] ; then
     export nodes=$((nodes+1))
   fi
   np="$numproc/$nodes/N"

   if [ $machine = WCOSS_C ]; then
    rmem=$((memory_node*nthreads/npe_node))/$nthreads/$pe_node
    re="${EOMG_MEMORY:-$rmem}"
   else
    re="$((memory_node*nthreads/npe_node))/$nthreads/$pe_node"
   fi
   tl=${TIMELIMEOMG:-1:00:00}
   wr="+0000"

   if [ $machine = IBMP6 ]; then
     en=CONFIG,CDATE,CDUMP,CSTEP,RUN_ENVIR,ENSBEG,ENSEND,ENSGRP
     $SUB -i -a $ac -e $en -g $gr -j $jn -o $df -p $np -q $qn -r $re -t $tl -u $us -w $wr $job
     rc=$?

   else
     if [ $nthreads -eq 1 ] ; then
       OMP_NUM_THREADS=$nthreads
       APRUN="$launcher"
       if [ $machine = WCOSS_C ]; then
          APRUN="'$launcher -j 1 -n $numproc -N $pe_node -d $nthreads -cc depth'"
       elif [ $machine = THEIA ]; then
         np="$npe_node/$nodes/N"
         APRUN="'$launcher -np $numproc'"
       fi
     else
       OMP_NUM_THREADS=$nthreads
       APRUN="$launcher"
       if [ $machine = WCOSS_C ]; then
          APRUN="'$launcher -j 1 -n $numproc -N $pe_node -d $nthreads -cc depth'"
       elif [ $machine = THEIA ]; then
         APRUN="'$launcher -np $numproc $omplace -nt $nthreads'"
       fi
     fi
     if [ $nthreads -eq 1 -o $machine = THEIA ] ; then
       np="$npe_node/$nodes/N"
       APRUN="'$launcher -np $numproc'"
     fi
     APRUN=${APRUN:-""}
     APRUNC=${APRUNC:-""}
     APRUNCY=${APRUNCY:-""}
     OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
     CYCLETHREAD=${CYCLETHREAD:-1}
     CLDASSH=${CLDASSH:-""}
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
     echo "export CLDASSH=$CLDASSH"                 >> $config_local
     echo "export CYCLETHREAD=$CYCLETHREAD"         >> $config_local
     echo "export ENSBEG=$ENSBEG"                   >> $config_local
     echo "export ENSEND=$ENSEND"                   >> $config_local
     echo "export ENSGRP=$ENSGRP"                   >> $config_local
     echo "export SUBMIT_OMN=$SUBMIT_OMN"           >> $config_local
     cat $CONFIG                                    >> $config_local
     chmod 755 $config_local

     en=CONFIG="$config_local"

     count=0
     if [ -s ${EOMGGRP}${igrp} ]; then count=`cat  ${EOMGGRP}${igrp} | wc -l` ;fi
     if [ ! -s  ${EOMGGRP}${igrp} -o $count -lt $nstep ]; then
       errchk=1
       if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
         jobstatus=$( $LLQU -u $USER -w | grep $jn )
         errchk=$?
       elif [ $machine = THEIA ] ; then
         jobstatus=$( $LLQU | grep $jn | grep -v " C " )
         errchk=$?
       fi
       if [ $errchk -eq 0 ] ; then
        $PLOG "$RUNLOG" NOTE "$jn already queued or running"
       else
        $SUB -a $ac -e $en -j $jn -o $df -p $np -r $re -q $qn -t $tl $job
        rc=$?
	$PLOG "$RUNLOG" OK "$jn submitted"
       fi
     fi
   fi

   if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

   (( nbeg = $nend + 1 ))
   (( igrp = $igrp + 1 ))
done

fi

################################################################################
# Check for completion of all ensemble forecast jobs
# Loop below is set up for a maximum of 90*10=900 = 15m wait
nsleep=0
tsleep=10
msleep=90
rm eomg_status
while [[ $nsleep -le $msleep ]]; do 
   rm eomg_status
   cat ${EOMGGRP}* > eomg_status
   if [[ -s eomg_status ]]; then
      count=`cat eomg_status  | wc -l` 
      if [[ $count -eq $NMEM_ENKF  ]]; then
         ${NCP:-/bin/cp} eomg_status $EOMNSTAT
         break
      fi
   fi
   sleep $tsleep
   (( nsleep = $nsleep + 1 ))
done

if [[ -s $EOMNSTAT ]]; then
   size=`cat $EOMNSTAT | wc -l`
   if [[ $size -ne $NMEM_ENKF ]]; then
      echo "***WARNING*** $size entries in $EOMNSTAT instead of $END_NUM_ENKF"
      exit 98
   fi
else
   echo "***WARNING*** file $EOMNSTAT does not exist"
   export SUBMIT_OMN=NO

   export CSTEP=$CSTEPIN

   jn=$PSLOT$CDATE$CDUMP$CSTEP
   df=$COMDAY/$jn.dayfile
   count=`ls ${df}* | wc -l`
   if [ $count -gt 0 ] ; then
     if [ $count -gt $RUNLIMEOMN ] ; then
       $PLOG "$RUNLOG" ERROR "$jn reached $RUNLIMEOMN run limit"
       exit 8
     fi
     if [ $count -gt 2 ]; then export SUBMIT_OMN=YES ; fi
     suffix=`expr $count + 1`
     df=$COMDAY/${jn}.dayfile${suffix}
   fi

   ac=$ACCOUNT
   gr=$GROUP
   us=$USER

   job=$EOMNSH

   cue2run=$CUE2RUN1
   numproc=1
   nodes=1
   npe_node=1
   np="$numproc/$nodes/N"
   re="1000/1"
   timelim=${TIMELIMEOMG:-1:00:00}
   whenrun=+0000

   qn=$cue2run
   tl=$timelim
   wr=$whenrun

   if [ $machine = IBMP6 ]; then
     en=CONFIG,CDATE,CDUMP,CSTEP,RUN_ENVIR,SUBMIT_OMN
     $SUB -i -a $ac -e $en -g $gr -j $jn -o $df -p $np -q $qn -r $re -t $tl -u $us -w $wr $job
     rc=$?

   else
     APRUN="'$launcher $numproc'"
     if [ $machine = THEIA ]; then
       np="$npe_node/$nodes/N"
       APRUN="'$launcher -np $numproc'"
     fi
     APRUN=${APRUN:-""}
     APRUNC=${APRUNC:-""}
     APRUNCY=${APRUNCY:-""}
     OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
     CYCLETHREAD=${CYCLETHREAD:-1}
     CLDASSH=${CLDASSH:-""}
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
     echo "export CLDASSH=$CLDASSH"                 >> $config_local
     echo "export CYCLETHREAD=$CYCLETHREAD"         >> $config_local
     echo "export ENSBEG=$ENSBEG"                   >> $config_local
     echo "export ENSEND=$ENSEND"                   >> $config_local
     echo "export ENSGRP=$ENSGRP"                   >> $config_local
     echo "export SUBMIT_OMN=$SUBMIT_OMN"           >> $config_local
     cat $CONFIG                                    >> $config_local
     chmod 755 $config_local

     en=CONFIG="$config_local"

     $SUB -a $ac -e $en -j $jn -o $df -p $np -r $re -q $qn -t $tl $job
     rc=$?

   fi
   if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
   $PLOG "$RUNLOG" OK "$jn submitted"

   $PLOG "$RUNLOG" OK "$jn ended"
   exit 0
   
fi

################################################################################
# Copy out restart and output files

export CSTEP=$CSTEPIN

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
