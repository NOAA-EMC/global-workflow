#!/bin/ksh
################################################################################
# This script runs ensemble member innovations for enkf GDAS.
# Usage: eomg.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELOMG
#   PMDLOGANAL
#   DATATMP
#   COMIN
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
export CSTEPCOP=eomg
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')

eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
DATATOP=$DATA
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
export WGRIB=${WGRIB:-${NWPROD}/util/exec/wgrib}
export ANALYSISSH=${ANALYSISSH:-$SCRDIR/exglobal_analysis.sh.sms}
export ENKFINVOBSSH=${ENKFINVOBSSH:-$SCRDIR/exglobal_enkf_innovate_obs.sh.sms}
export CONVINFO=${CONVINFO:-${FIXgsm}/global_convinfo.txt}
export INSITUINFO=${INSITUINFO:-${FIXgsm}/global_insituinfo.txt}
export OZINFO=${OZINFO:-${FIXgsm}/global_ozinfo.txt}
export PCPINFO=${PCPINFO:-${FIXgsm}/global_pcpinfo.txt}

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

elif [ $machine = WCOSS ] ; then
  export MP_EAGER_LIMIT=${MP_EAGER_LIMIT:-65536}
  export MP_COREFILE_FORMAT=${MP_COREFILE_FORMAT:-lite}
# export MP_EUIDEVICE=${MP_EUIDEVICE:-sn_all}
# export MP_EUILIB=${MP_EUILIB:-us}
  export MP_MPILIB=${MP_MPILIB:-mpich2}
  export MP_LABELIO=${MP_LABELIO:-yes}
  export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-yes}
  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  export MPICH_ALLTOALL_THROTTLE=${MPICH_ALLTOALL_THROTTLE:-0}
  export MP_COLLECTIVE_OFFLOAD=${MP_COLLECTIVE_OFFLOAD:-yes}
  export MP_SINGLE_THREAD=${MP_SINGLE_THREAD:-yes}
  export KMP_STACKSIZE=${KMP_STACKSIZE:-2048m}
  export NTHREADS_GSI=${NTHREADS_GSI:-1}
  . /usrx/local/Modules/3.2.10/init/ksh
  module load cfp
  export APRUNCFP=${APRUNCFP_EOMG:-"mpirun.lsf cfp"}
elif [ $machine = WCOSS_C ] ; then
  . $MODULESHOME/init/sh
  module load cfp-intel-sandybridge
  export APRUNCFP=${APRUNCFP_EOMG:-"aprun cfp"}
else
  export MPI_BUFS_PER_PROC=${MPI_BUFS_PER_PROC:-256}
  export MPI_BUFS_PER_HOST=${MPI_BUFS_PER_HOST:-256}
  export MPI_GROUP_MAX=${MPI_GROUP_MAX:-256}
  export MPI_MEMMAP_OFF=${MPI_MEMMAP_OFF:-1}
fi

export NTHREADS_GSI=${NTHREADS_EOMG:-1}
export NTHSTACK_GSI=${NTHSTACK_EOMG:-1024000000}


export PREINP=gdas1.t$(echo $CDATE|cut -c9-10)z.
export FILESTYLE=${FILESTYLEEOMG:-C}
export VERBOSE=YES
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export CYCLVARS=${CYCLVARS:-""}
export COMINGES=${COMINGES:-$COMROT}
export COMOUT=${COMOUT:-$COMROT}
COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
eval COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
#
#[[ -n ${COMDMPTMP:-""} ]]&&eval export COMDMP=$COMDMPTMP
#export COMDMPG=${COMDMPG:-$COMDMP}
#[[ -n ${COMDMPTMP:-""} ]]&&export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)

export COMIN_DUMP=${COMIN_DUMP:-$COMDMP}
export COMIN_DUMPG=${COMIN_DUMPG:-$COMDMPG}
export COMIN_GDAS=${COMIN_GDAS:-$COMROT}
export COMIN_ENKF=${COMIN_ENKF:-$COMROT}

#

[[ -n ${FNSNOAJCAP_ENKF_TMP:-""} ]]&&eval export FNSNOAJCAP_ENKF=$FNSNOAJCAP_ENKF_TMP
export FNSNOAJCAP=${FNSNOAJCAP_ENKF:-$DMPDIR/$CDATE/${CDUMP}/snogrb_t$JCAP_ENKF.$CDUMP.$CDATE}
if [ ! -s $FNSNOAJCAP ]; then export FNSNOAJCAP=$DMPDIR/$CDATE/${CDUMP}x/snogrb_t$JCAP_ENKF.$CDUMP.$CDATE ;fi
[[ -n ${FNSNOGJCAP_ENKF_TMP:-""} ]]&&eval export FNSNOGJCAP_ENKF=$FNSNOGJCAP_ENKF_TMP
export FNSNOGJCAP=${FNSNOGJCAP_ENKF:-$DMPDIR/$GDATE/${GDUMP}/snogrb_t$JCAP_ENKF.$GDUMP.$GDATE}
if [ ! -s $FNSNOGJCAP ]; then export FNSNOGJCAP=$DMPDIR/$GDATE/${GDUMP}x/snogrb_t$JCAP_ENKF.$GDUMP.$GDATE ;fi


export JCAP=${JCAP_ENKF:-254}
export JCAP_A=$JCAP
export LEVS=${LEVS_ENKF:-64}
export LONB=${LONB_ENKF:-768}
export LATB=${LATB_ENKF:-384}
export LONA=${LONA_ENKF:-512}
export LATA=${LATA_ENKF:-256}
export NLON_A=$LONA
export NLAT_A=$(($LATA+2))
export DELTIM=${DELTIM_ENKF:-300}

##export FNSNOAJCAP=${FNSNOAJCAP:-$DMPDIR/$CDATE/${CDUMP}/snogrb_t$JCAP.$CDUMP.$CDATE}
##if [ ! -s $FNSNOAJCAP ]; then export FNSNOAJCAP=$DMPDIR/$CDATE/${CDUMP}x/snogrb_t$JCAP.$CDUMP.$CDATE ;fi
##export FNSNOGJCAP=${FNSNOGJCAP:-$DMPDIR/$GDATE/${GDUMP}/snogrb_t$JCAP.$GDUMP.$GDATE}
##if [ ! -s $FNSNOGJCAP ]; then export FNSNOGJCAP=$DMPDIR/$GDATE/${GDUMP}x/snogrb_t$JCAP.$GDUMP.$GDATE ;fi

export BERROR=${BERROR_ENKF:-${FIXgsm}/global_berror.l${LEVS}y${NLAT_A}.f77}
export FNOROG=${FNOROG:-${FIXgsm}/global_orography.t$JCAP.grb}
export FNMASK=${FNMASK:-${FIXgsm}/global_slmask.t$JCAP.grb}
export OROGRAPHY=${OROGRAPHY:-${FIXgsm}/global_orography.t$JCAP.grb}
export SLMASK=${SLMASK:-${FIXgsm}/global_slmask.t$JCAP.grb}

export DOIAU=${DOIAU_ENKF:-"NO"}

################################################################################
# Copy in restart and input files

##$PCOP $CDATE/$CDUMP/$CSTEPCOP/ROTI $COMROT $DATA <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

##$PCOP $CDATE/$CDUMP/$CSTEPCOP/OPTI $COMROT $DATA <$RLIST
##$PCOP $CDATE/$CDUMP/$CSTEPCOP/DMPI $COMDMP $DATA <$RLIST
##$PCOP $CDATE/$CDUMP/$CSTEPCOP/DMPG $COMDMPG $DATA <$RLIST


export GBIAS=${GBIAS:-$COMIN_GDAS/biascr.$GDUMP.$GDATE}
export GBIASPC=${GBIASPC:-$COMIN_GDAS/biascr_pc.$GDUMP.$GDATE}
export GBIASAIR=${GBIASAIR:-$COMIN_GDAS/aircraft_t_bias.$GDUMP.$GDATE}
export GSATANG=${GSATANG:-$COMIN_GDAS/satang.$GDUMP.$GDATE}
export GRADSTAT=${GRADSTAT:-$COMIN_GDAS/radstat.$GDUMP.$GDATE}

export SIGGESMEAN=${SIGGESMEAN:-$COMIN_ENKF/sfg_${GDATE}_fhr06_ensmean}
export SFCGESMEAN=${SFCGESMEAN:-$COMIN_ENKF/bfg_${GDATE}_fhr06_ensmean}
export NSTGESMEAN=${NSTGESMEAN:-$COMIN_ENKF/nfg_${GDATE}_fhr06_ensmean}

export SELECT_OBS=${SELECT_OBS:-$COMIN_ENKF/obsinput_${CDATE}_ensmean}

# Make use of updated angle dependent bias file, if it exists.
if [[ -s $GSATANG ]]; then
   export SATANGL=$GSATANG
fi


################################################################################
# Set variables for global_cycle
[[ -n ${FNTSFATMP:-""} ]]&&eval export FNTSFA=$FNTSFATMP
export FNTSFA=${FNTSFA:-$COMIN_DUMP/sstgrb.$CDUMP.$CDATE}

[[ -n ${FNACNATMP:-""} ]]&&eval export FNACNA=$FNACNATMP
export FNACNA=${FNACNA:-$COMIN_DUMP/icegrb.$CDUMP.$CDATE}

export FNSNOA=${FNSNOA:-$COMIN_DUMP/snogrb.$CDUMP.$CDATE}
export FNSNOG=${FNSNOG:-$COMIN_DUMPG/snogrb.$GDUMP.$GDATE}

# If snogrb_t$JCAP files exists, use it.   If it does not exist and
# if USE_JCAP_SNO=YES (default) then abort. Otherwise, revert to snogrb files

if [[ -s $FNSNOAJCAP ]]; then
   eval export FNSNOA=$FNSNOAJCAP
else
   if [ ${USE_JCAP_SNO:-YES} = YES ] ; then
     echo $FNSNOAJCAP 'does not exit'
     exit 111
   fi
fi
if [[ -s $FNSNOGJCAP ]]; then
   eval export FNSNOG=$FNSNOGJCAP
else
   if [ ${USE_JCAP_SNO:-YES} = YES ] ; then
     echo $FNSNOGJCAP 'does not exist'
     exit 222
   fi
fi

#
if [ ! -s $FNSNOA ] ; then
 FNSNOA=${FNSNOA}.r2
fi
xx=$($WGRIB -4yr $FNSNOA | awk -F: '{print $4}')
snoid=NONE
for snovar in $xx ; do
 if [ $snovar = SNOD ] ; then
  snoid=SNOD
 elif [ $snovar = WEASD ] ; then
  snoid=WEASD
 fi
done
if [ $snoid = NONE ] ; then
 for snovar in $xx ; do if [ $snovar = SNOWC ] ; then snoid=SNOWC ; fi ; done
fi
#snoid=$($WGRIB -4yr $FNSNOA | awk -F: '{print $4}')

################################################################################
# Turn off snow analysis if it has already been used.
if [ `$WGRIB -4yr ${FNSNOA} 2>/dev/null|grep -i $snoid |\
	  awk -F: '{print $3}'|awk -F= '{print $2}'` -le \
     `$WGRIB -4yr ${FNSNOG} 2>/dev/null |grep -i $snoid  |\
			awk -F: '{print $3}'|awk -F= '{print $2}'` ] ; then
	export FNSNOA=" "
	export CYCLVARS="FSNOL=99999.,FSNOS=99999.,$CYCLVARS"
else
     export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-0.}
     export CYCLVARS="FSNOL=${SNOW_NUDGE_COEFF},$CYCLVARS"
fi
#
#  If the snow file contains snow cover, define FNSCVA and remove FNSNOA
#
if [ $snoid = SNOWC ] ; then
#export FNSCVA=$FNSNOA
 export CYCLVARS="FNSCVA=\"$FNSNOA\",$CYCLVARS"
 export FNSNOA=" "
fi


################################################################################
# Generate ensemble innovations

$ENKFINVOBSSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi


################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND
