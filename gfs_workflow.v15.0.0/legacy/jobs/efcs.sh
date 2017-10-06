#!/bin/ksh
################################################################################
# This script runs ensemble member forecasts for enkf GDAS
# Usage: efcs.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELFCS
#   PMDLOGANAL
#   FNTSFATMP
#   SMIPCPTMP
#   TMIPCPTMP
#   DATATMP
#   COMIN
#   COMRS
#   COMROT
#   NCP
#   FORECASTSH
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
export CSTEPCOP=efcs
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

export FILESTYLE=${FILESTYLEEFCS:-'L'}

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export ENKFFCSTSH=${ENKFFCSTSH:-$SCRDIR/exglobal_enkf_fcst.sh.sms}
export FORECASTSH=${FORECASTSH:-$SCRDIR/exglobal_fcst.sh.sms}
export VERBOSE=YES
export CDFNL=${CDFNL:-gdas}

export COMIN=${COMIN:-$COMROT}
export COMOUT=${COMOUT:-$COMROT}

#
export NTHREADS=${NTHREADS_EFCS:-2}
export NTHSTACK=${NTHSTACK_EFCS:-1024000000}
export tasks=$(eval echo \${NUMPROCEFCS$CDUMP:-${NUMPROCEFCS:-"$(($JCAP/2+3))"}})
export NUMTHRD=${NTHREADS_EFCS:-1}
export ENS_NUM=${ENS_NUM:-1}


[[ -n ${AMEXECTMP:-""} ]]&&eval export AM_EXEC=$AMEXECTMP
export FCSTEXEC=$AM_EXEC

############################################################################
# Set NEMS settings

export GEFS_ENSEMBLE=${GEFS_ENSEMBLE:-0}
echo "GEFS_ENSEMBLE=" $GEFS_ENSEMBLE

export NPROCS_a=$tasks
task_mem=$((NPROCS_a/ENS_NUM))
c=1
while [ $c -le $ENS_NUM ] ; do
 export PE$c=$task_mem
 c=$((c+1))
done
export PE1=${PE1:-$tasks}
export nodes=$((tasks*NUMTHRD/npe_node_efcs))
#
export chgres_only=${chgres_only:-NO}
if [ $chgres_only = YES ] ; then
 tasks=1  ; export PE1=1 ; export nodes=1 ; export pe_node=1
 rcpu=$rcpu_chgres
#class=debug
fi

if [ $nodes = 0 ] ; then export nodes=1 ; fi
if [ $((nodes*npe_node_efcs/NUMTHRD)) -lt $tasks ] ; then
  export nodes=$((nodes+1))
elif [ $((nodes*pe_node)) -lt $tasks ] ; then
  export nodes=$((nodes+1))
fi
if [ $((tasks/ENS_NUM)) -gt $JCAP ] ; then tasks=$((JCAP-1)) ; PE1=$tasks ; fi
export mpi_tasks=$tasks
export size=$((nodes*npe_node_efcs))


################################################################################
# Copy in restart and input files

##$PCOP $CDATE/$CDUMP/$CSTEPCOP/ROTI $COMROT $DATA <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

##$PCOP $CDATE/$CDUMP/$CSTEPCOP/OPTI $COMROT $DATA <$RLIST


################################################################################
# Run ensemble forecasts

$ENKFFCSTSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi


################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND
