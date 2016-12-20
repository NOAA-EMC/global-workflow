#!/bin/ksh
################################################################################
# This script recenters enkf analysis about hires analysis
# Usage: ecen.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMROT
#   NCP
#   NDATE
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

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export NCP=${NCP:-cp}
export ENKFINFCENSH=${ENKFINFCENSH:-$SCRDIR/exglobal_enkf_inflate_recenter.sh.sms}
export CHGRESSH=${CHGRESSH:-${NWPROD}/ush/global_chgres.sh}
export PERTURBDIR=${PERTURBDIR:-${FIXgsm}/enkf_gfs}
export PBDATE=${PBDATE:-2010050100}
export PEDATE=${PEDATE:-2011043018}
export SWITCH=${SWITCH:-043018}

export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-fnl}
export GDUMP=${GDUMP:-$CDFNL}
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')

export NET=gdas
export RUN=gdas1
export envir=prod
export FILESTYLE=${FILESTYLEECEN:-C}
export COMSP=$DATA/
export COMIN=${COMIN:-$COMROT}
export COMOUT=${COMOUT:-$COMROT}
export SENDCOM=${SENDCOM:-YES}
export GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-$EXECDIR/getsfcensmean.x}
export GETNSTENSMEANEXEC=${GETNSTENSMEANEXEC:-$EXECDIR/getnstensmean.x}
export GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-$EXECDIR/getsigensmean_smooth.x}
export GETSFCNSTENSUPDEXEC=${GETSFCNSTENSUPDEXEC:-$EXECDIR/getsfcnstensupd.x}

export IDRT=${IDRT_ECEN:-4}

if [ $machine = WCOSS ] ; then
 export MP_LABELIO=${MP_LABELIO:-yes}
 export MP_EAGER_LIMIT=${MP_EAGER_LIMIT:-32768}
 export MP_COREFILE_FORMAT=${MP_COREFILE_FORMAT:-lite}
 export MP_MPILIB=${MP_MPLIB:-mpich2}
 export MP_SINGLE_THREAD=${MP_SINGLE_THREAD:-yes}
 export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-no}
 export MPICH_ALLTOALL_THROTTLE=${MPICH_ALLTOALL_THROTTLE:-0}
 export MP_COLLECTIVE_OFFLOAD=NO    # must be NO; otherwise results not reproducible
 export KMP_STACKSIZE=${KMP_STACKSIZE:-1024m}
 export CHGRESTHREAD=${CHGRESTHREAD:-16}
fi

#############################
# Set up the UTILITIES
##############################
export ushscript=${USHGLOBAL:-${NWPROD}/ush}
export utilscript=${USHUTIL:-${NWPROD}/util/ush}
export utilities=${USHUTIL:-${NWPROD}/util/ush}
export jlogfile=${jlogfile:-""}

export pgmout=stdout

################################################################################
# Copy in restart and input files


##$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

##$PCOP $CDATE/$CDUMP/$CSTEP/OPTI $COMROT $DATA <$RLIST

# Input files copied via ENKFINFCENSH
export SIGANL_HI=${SIGANL_HI:-${COMIN}/$SIGISUF.$CDUMP.$CDATE}
export SFCANL_HI=${SFCANL_HI:-${COMIN}/$SFCISUF.$CDUMP.$CDATE}
export SIGGES=${SIGGES:-${COMIN}/sfg_${GDATE}_fhr06_ensmean}
export SIGANLENS_IN=${SIGANLENS_IN:-${COMIN}/sanl_${CDATE}}
export DTFANL=${DTFANL:-${COMIN}/dtfanl.$CDUMP.$CDATE}

# Output files copied out of $DATA via ENKFINFCENSH
# Thus, include path COMOUT in default
export PERTDATES=${PERTDATES:-$COMOUT/pertdates_$CDATE}
export SANLENSMEAN=${SANLENSMEAN:-$COMOUT/sanl_${CDATE}_ensmean}
export SIGANLENS_OUT=${SIGANLENS_OUT:-${COMOUT}/siganl_${CDATE}}


################################################################################
# Run additive inflation and recenter script

$ENKFINFCENSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi


################################################################################
# Copy output files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

##$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND
