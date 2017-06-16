#!/bin/ksh
################################################################################
# This script runs the analysis.
# Usage: anal.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELANAL
#   PMDLOGANAL
#   FNTSFATMP
#   SMIPCPTMP
#   TMIPCPTMP
#   DATATMP
#   COMIN
#   COMRS
#   COMROT
#   NCP
#   NDATE
#   ANALYSISSH
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
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
export machine=${machine:-GAEA}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
#
export BASEDIR=${BASEDIR:-..}
export SCRDIR=${SCRDIR:-$BASEDIR/scripts}
export FIXDIR=${FIXDIR:-$BASEDIR/fix/fix_am}
export FIXgsm=${FIXgsm:-$FIXDIR}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
#             Other option for snoid is weasd (for old snow files)
export snoid=${snoid:-snod}

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export WGRIB=${WGRIB:-${NWPROD}/util/exec/wgrib}
export ANGUPDATESH=${ANGUPDATESH:-$SCRDIR/exglobal_angupdate.sh.sms}
#
if [ $machine = IBMP6 ] ; then
  export MP_INFOLEVEL=${INFOLEVELANAL:-2}
  export MP_PMDLOG=${PMDLOGANAL:-no}
  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  export MEMORY_AFFINITY=${MEMORY_AFFINITY:-MCM}
  export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-no}
  export BIND_TASKS=${BIND_TASKS:-NO}
  export MP_LABELIO=${MP_LABELIO:-yes}
  export MP_COREFILE_FORMAT=lite
##TEST
# Environment variables from Carolyn
  export LAPI_DEBUG_ENABLE_AFFINITY=YES
# export LAPI_DEBUG_MTU_4K=YES
  export MP_FIFO_MTU=4K
  export MP_SYNC_QP=YES
  export MP_RFIFO_SIZE=16777216
  export MP_SHM_ATTACH_THRESH=500000
  export MP_EUIDEVELOP=min
#RDMA specific tunables:
  export MP_USE_BULK_XFER=yes
  export MP_BULK_MIN_MSG_SIZE=64k
  export MP_RC_MAX_QP=8192
  export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
  export LAPI_DEBUG_QP_NOTIFICATION=no
  export LAPI_DEBUG_RC_INIT_SETUP=yes
elif [ $machine = GAEA ] ; then
  export MPICH_FAST_MEMCPY=${MPICH_FAST_MEMCPY:-"ENABLE"}
  export MPICH_UNEX_BUFFER_SIZE=${MPICH_UNEX_BUFFER_SIZE:-1025000000}
  export MPICH_MAX_SHORT_MSG_SIZE=${MPICH_MAX_SHORT_MSG_SIZE:-8192}
  export MPICH_PTL_UNEX_EVENTS=${MPICH_PTL_UNEX_EVENTS:-64000}
else
  export MPI_BUFS_PER_PROC=${MPI_BUFS_PER_PROC:-256}
  export MPI_BUFS_PER_HOST=${MPI_BUFS_PER_HOST:-256}
  export MPI_GROUP_MAX=${MPI_GROUP_MAX:-256}
fi

export PREINP=gdas1.t$(echo $CDATE|cut -c9-10)z.
export FILESTYLE=${FILESTYLEANGU:-'C'}
export VERBOSE=YES
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export CYCLVARS=${CYCLVARS:-""}
COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
eval COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
#
#[[ -n ${COMDMPTMP:-""} ]]&&eval export COMDMP=$COMDMPTMP
#export COMDMPG=${COMDMPG:-$COMDMP}
#[[ -n ${COMDMPTMP:-""} ]]&&export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
#

export COMIN_GDAS=${COMIN_GDAS:-$COMROT}
export COMOUT_GDAS=${COMOUT_GDAS:-$COMROT}

export NTHREADS_GSI=${NTHREADS_GSI:-1}
export CDATE_SKIP=${CDATE_SKIP:-0}


################################################################################
# Copy in restart and input files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

export GSATANG=${GSATANG:-$COMIN_GDAS/satang.$GDUMP.$GDATE}
export SFCGES=${SFCGES:-$COMIN_GDAS/${SFCOSUF}f06.$GDUMP.$GDATE}
export RADSTAT=${RADSTAT:-$COMIN_GDAS/radstat.$CDUMP.$CDATE}
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $GDUMP|tr '[a-z]' '[A-Z]')
eval mlanl=\${MLANL$cycle$cdump:-0}
if [ $mlanl -gt 0 -a $CDATE -gt $CDATE_SKIP ] ; then
 tsleep=10
 msleep=120
 nsleep=0
 until [[ -s $COMROT/$(basename $SFCGES.LIS) || $((nsleep+=1)) -gt $msleep ]];do
   sleep $tsleep
 done
 if [[ $nsleep -gt $msleep ]] ; then
   echo 'NO SFC FILE FROM LDAS - JOB STOPPED'
   $PERR
   exit 2
 fi
 export SFCGES=$SFCGES.LIS
 ${NCP:-/bin/cp} $COMROT/$(basename $SFCGES) $SFCGES
fi

################################################################################
# Make use of updated angle dependent bias file, if it exists.

if [[ -s $GSATANG ]]; then
   export SATANGL=$GSATANG
fi

################################################################################
# Set output data

export SATANGO=${SATANGO:-$COMOUT_GDAS/satang.$CDUMP.$CDATE}


################################################################################
# Run angupdate

$ANGUPDATESH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

################################################################################
# Copy out restart and output files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
##rc=$?
##$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
