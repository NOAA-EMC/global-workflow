#!/bin/ksh
################################################################################
# This script submits and monitors ensemble member forecasts for enkf GDAS.
# Usage: efmn.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   PMDLOGANAL
#   DATATMP
#   COMIN
#   COMRS
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
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
export CSTEPIN=$CSTEP
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
if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
 export LLQU=${LLQU:-bjobs}
elif [ $machine = THEIA ] ; then
 export LLQU=${LLQU:-$SHDIR/llqu}
else
 export LLQU=${LLQU:-""}
fi

#
if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
  if [ $machine = WCOSS ]; then
     export launcher=${launcher:-mpirun.lsf}
     export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-yes}
     export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  elif [ $machine = WCOSS_C ]; then
     export launcher=${launcher:-aprun}
     export MKL_CBWR=AVX
     export ATP_ENABLED=0
     export KMP_AFFINITY=disabled
     export KMP_STACKSIZE=2048M
  fi
####  export launcher=${launcher:-mpirun.lsf}
####  export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-yes}
####  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
elif [ $machine = THEIA ] ; then
  export launcher=${launcher:-mpirun}
  export omplace=${omplace:-""}
elif [ $machine = IBMP6 ] ; then
  export launcher=${launcher:-""}
elif [ $machine = GAEA ] ; then
  export launcher=${launcher:-aprun}
fi

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export memory_node=${memory_node:-105000}

export VERBOSE=YES
export CDFNL=${CDFNL:-gdas}

export NTHREADS=${NTHREADS_EFCS:-2}

export SUBMIT_FMN=${SUBMIT_FMN:-YES}
export RUNLIMEFMN=${RUNLIMEFMN:-25}

rc=0

################################################################################
# Copy in restart and input files

export JCAP=${JCAP_ENKF:-254}
export JCAP_A=$JCAP
export LEVS=${LEVS_ENKF:-64}
export LONB=${LONB_ENKF:-768}
export LATB=${LATB_ENKF:-384}
export LONA=${LONA_ENKF:-512}
export LATA=${LATA_ENKF:-256}
export NLON_A=$LONA
export NLAT_A=$(($LATA+2))
export DELTIM=${DELTIM_ENKF:-900}
export DTPHYS=${DTPHYS_ENKF:-300}

export FNOROG=${FNOROG_ENKF:-$FIXgsm/global_orography.t$JCAP.grb}
export FNMASK=${FNMASK_ENKF:-$FIXgsm/global_slmask.t$JCAP.grb}
export OROGRAPHY=${OROGRAPHY_ENKF:-$FIXgsm/global_orography.t$JCAP.grb}
export OROGRAPHY_UF=${OROGRAPHY_UF_ENKF:-$FIXgsm/global_orography.t$JCAP.$LONB.$LATB.grb}
export SLMASK=${SLMASK_ENKF:-$FIXgsm/global_slmask.t$JCAP.grb}
export MTNVAR=${MTNVAR_ENKF:-$FIXgsm/global_mtnvar.t$JCAP.f77}

################################################################################
# Set output data

EFMNSTAT=$COMROT/fcsstat_${CDATE}_all
EFCSGRP=$COMROT/fcsstat_${CDATE}_grp


################################################################################
# If requested, submit ensemble forecast jobs
if [[ $SUBMIT_FMN = YES ]]; then

rm -f $EFMNSTAT
#rm -f ${EFCSGRP}*

rc=0
export NMEM_ENKF=${NMEM_ENKF:-80}
export nstep=${NMEM_ENKF_GRP:-1}
(( ninc = $nstep - 1 ))
export igrp=1
export nbeg=1
while [[ $nbeg -le $NMEM_ENKF ]]; do
   (( nend = $nbeg + $ninc ))
   export ENSBEG=$nbeg
   export ENSEND=$nend
   export ENSGRP=$igrp
   export CSTEP="efmn"`printf %02i $igrp`
   ac=$ACCOUNT
   gr=$GROUP
   us=$USER
   qn=$CUE2RUN

   jn=$PSLOT$CDATE$CDUMP$CSTEP
   df=$COMDAY/$jn.dayfile

   if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
      npe_node_efcs_default=16
   elif [ $machine = THEIA ] ; then
      npe_node_efcs_default=12
   elif [ $machine = IBMP6 ] ; then
      npe_node_efcs_default=32
   fi

   job=$EFCSSH

   npe_node=${npe_node_efcs:-$npe_node_efcs_default}
   pe_node=${pe_node_efcs:-$npe_node}
   prereqs="true"
   numproc=$(eval echo \${NUMPROCEFCS$CDUMP:-${NUMPROCEFCS:-"$(($JCAP/2+3))"}})
   nthreads=${NTHREADS_EFCS:-1}
   if [ $pe_node -eq $npe_node -a $nthreads -gt 1 ] ; then
     pe_node=$((npe_node/nthreads))
   fi
   nodes=$((numproc/pe_node))
   if [ $numproc -gt $((nodes*pe_node)) ] ; then
     export nodes=$((nodes+1))
   fi
   np="$numproc/$nodes/N"

   if [ $machine = WCOSS_C ]; then
    rmem=$((memory_node*nthreads/npe_node))
    re="${EFCS_MEMORY:-$rmem}/$nthreads/$pe_node"
   else
    re="$((memory_node*nthreads/npe_node))/$nthreads/$pe_node"
   fi

   tl=${TIMELIMEFCS:-1:00:00}
   wr="+0000"

   if [ $machine = IBMP6 ]; then
     en=CONFIG,CDATE,CDUMP,CSTEP,RUN_ENVIR,ENSBEG,ENSEND,ENSGRP
     np="$numproc/$nodes/N"
     $SUB -i -a $ac -e $en -g $gr -j $jn -o $df -p $np -q $qn -r $re -t $tl -u $us -w $wr $job
     rc=$?

   else
     OMP_NUM_THREADS=$nthreads
     APRUN="$launcher"
     if [ $machine = WCOSS_C ] ; then
       APRUN="'$launcher -j 1 -n $numproc -N $pe_node -d $nthreads -cc depth'"
     fi
     if [ $machine = THEIA ] ; then
       np="$npe_node/$nodes/N"
       if [ $nthreads -eq 1 ] ; then
         APRUN="'$launcher -np $numproc'"
       else
         APRUN="'$launcher -np $numproc $omplace -nt $nthreads'"
       fi
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
     echo "export SUBMIT_FMN=$SUBMIT_FMN"           >> $config_local
     cat $CONFIG                                    >> $config_local
     chmod 755 $config_local

     en=CONFIG="$config_local"

     count=0
     if [ -s ${EFCSGRP}${igrp} ]; then count=`cat  ${EFCSGRP}${igrp} | wc -l` ;fi
     if [ ! -s  ${EFCSGRP}${igrp} -o $count -lt $nstep ]; then
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
# Check for completion of all ensemble innovation jobs
# Loop below is set up for a maximum of 90*10=900 = 15m wait
nsleep=0
tsleep=10
msleep=90
rm efcs_status
while [[ $nsleep -le $msleep ]]; do 
   rm efcs_status
   cat ${EFCSGRP}* > efcs_status
   if [[ -s efcs_status ]]; then
      count=`cat efcs_status  | wc -l` 
      if [[ $count -eq $NMEM_ENKF  ]]; then
         ${NCP:-/bin/cp} efcs_status $EFMNSTAT
         break
      fi
   fi
   sleep $tsleep
   (( nsleep = $nsleep + 1 ))
done

if [[ -s $EFMNSTAT ]]; then
   size=`cat $EFMNSTAT | wc -l`
   size_FAIL=`cat $EFMNSTAT | grep FAIL | wc -l`
   if [ $size -ne $NMEM_ENKF -o $size_FAIL -ne 0 ]; then
      if [ $size -ne $NMEM_ENKF ]; then
         echo "***WARNING*** $size entries in $EFMNSTAT instead of $NMEM_ENKF"
      fi
      if [ $size_FAIL -ne 0 ]; then
         echo "***FAILURE*** $size_FAIL out of $size members failed"
         cat $EFMNSTAT | grep 'FAIL'
         $PERR
         $PLOG "$RUNLOG" FAILURE "$jn failed, $size_FAIL / $size members"
      fi
      exit 98
   fi
else
   echo "***WARNING*** file $EFMNSTAT does not exist"
   export SUBMIT_FMN=NO

   export CSTEP=$CSTEPIN

   jn=$PSLOT$CDATE$CDUMP$CSTEP
   df=$COMDAY/$jn.dayfile
   count=`ls ${df}* | wc -l`
   if [ $count -gt 0 ] ; then
     if [ $count -gt $RUNLIMEFMN ] ; then
       $PLOG "$RUNLOG" ERROR "$jn reached $RUNLIMEFMN run limit"
       exit 8
     fi
     if [ $count -gt 2 ]; then  export SUBMIT_FMN=YES ;fi
     suffix=`expr $count + 1`
     df=$COMDAY/${jn}.dayfile${suffix}
   fi

   ac=$ACCOUNT
   gr=$GROUP
   us=$USER

   job=$EFMNSH

   cue2run=$CUE2RUN1
   numproc=1
   nodes=1
   npe_node=1
   np="$numproc/$nodes/N"
   re="1000/1"
   timelim=${TIMELIMEFMN:-1:00:00}
   whenrun=+0000

   qn=$cue2run
   tl=$timelim
   wr=$whenrun
  
   if [ $machine = IBMP6 ] ; then
     en=CONFIG,CDATE,CDUMP,CSTEP,RUN_ENVIR,SUBMIT_FMN
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
     echo "export SUBMIT_FMN=$SUBMIT_FMN"           >> $config_local
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
