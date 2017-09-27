#!/bin/ksh
################################################################################
# This script runs the ensemble mean data selection for enkf GDAS.
# Usage: eobs.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELANAL
#   PMDLOGANAL
#   DATATMP
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
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
#
export BASEDIR=${BASEDIR:-/nwprod}
export SCRDIR=${SCRDIR:-$BASEDIR/scripts}
export FIXDIR=${FIXDIR:-$BASEDIR/fix/fix_am}
export FIXgsm=${FIXgsm:-$FIXDIR}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export EXECDIR=${EXECDIR:-$BASEDIR/exec}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-/bin/cp}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export WGRIB=${WGRIB:-${NWPROD}/util/exec/wgrib}
export ANALYSISSH=${ANALYSISSH:-$SCRDIR/exglobal_analysis.sh.ecf}
export CONVINFO=${CONVINFO:-${FIXgsm}/global_convinfo.txt}
export INSITUINFO=${INSITUINFO:-${FIXgsm}/global_insituinfo.txt}
export OZINFO=${OZINFO:-${FIXgsm}/global_ozinfo.txt}
export PCPINFO=${PCPINFO:-${FIXgsm}/global_pcpinfo.txt}

if [ $machine = IBMP6 ] ; then
  export MP_INFOLEVEL=${INFOLEVELOBS:-2}
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
  export NTHREADS_GSI=${NTHREADS_GSI:-2}
  . /usrx/local/Modules/3.2.10/init/ksh
  module load cfp
  export APRUNCFP=${APRUNCFP_EOBS:-"mpirun.lsf cfp"}
elif [ $machine = WCOSS_C ] ; then
  . $MODULESHOME/init/sh
  module load cfp-intel-sandybridge
  export APRUNCFP=${APRUNCFP_EOBS:-"aprun cfp"}

else
  export MPI_BUFS_PER_PROC=${MPI_BUFS_PER_PROC:-256}
  export MPI_BUFS_PER_HOST=${MPI_BUFS_PER_HOST:-256}
  export MPI_GROUP_MAX=${MPI_GROUP_MAX:-256}
  export MPI_MEMMAP_OFF=${MPI_MEMMAP_OFF:-1}
fi

export NTHREADS_GSI=${NTHREADS_EOBS:-1}
export NTHSTACK_GSI=${NTHSTACK_EOBS:-1024000000}


export PREINP=gdas1.t$(echo $CDATE|cut -c9-10)z.
export FILESTYLE=${FILESTYLEEOBS:-C}
export VERBOSE=YES
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export CYCLVARS=${CYCLVARS:-""}
COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
eval COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)

COMDAYTMP=${COMDAYTMP:-${COMDAY:-""}}
eval export COMDAY=$COMDAYTMP

export COMIN_DUMP=${COMIN_DUMP:-$COMDMP}
export COMIN_DUMPG=${COMIN_DUMPG:-$COMDMPG}
export COMIN_GDAS=${COMIN_GDAS:-$COMROT}
export COMIN_ENKF=${COMIN_ENKF:-$COMROT}
export COMOUT_ENKF=${COMOUT_ENKF:-$COMROT}

#
#[[ -n ${COMDMPTMP:-""} ]]&&eval export COMDMP=$COMDMPTMP
#export COMDMPG=${COMDMPG:-$COMDMP}
#[[ -n ${COMDMPTMP:-""} ]]&&export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
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

export BERROR=${BERROR_ENKF:-${FIXgsm}/global_berror.l${LEVS}y${NLAT_A}.f77}
export FNOROG=${FNOROG:-${FIXgsm}/global_orography.t$JCAP.grb}
export FNMASK=${FNMASK:-${FIXgsm}/global_slmask.t$JCAP.grb}
export OROGRAPHY=${OROGRAPHY:-${FIXgsm}/global_orography.t$JCAP.grb}
export SLMASK=${SLMASK:-${FIXgsm}/global_slmask.t$JCAP.grb}

export DOIAU=${DOIAU_ENKF:-"NO"}

################################################################################
# Copy in restart and input files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi


export SFCGES=${SFCGES:-$COMIN_GDAS/${SFCOSUF}f06.$GDUMP.$GDATE}

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
 $NCP $COMROT/$(basename $SFCGES) $SFCGES
fi

##$PCOP $CDATE/$CDUMP/$CSTEP/OPTI $COMROT $DATA <$RLIST
##$PCOP $CDATE/$CDUMP/$CSTEP/DMPI $COMDMP $DATA <$RLIST
##$PCOP $CDATE/$CDUMP/$CSTEP/DMPG $COMDMPG $DATA <$RLIST


export GBIAS=${GBIAS:-$COMIN_GDAS/biascr.$GDUMP.$GDATE}
export GBIASPC=${GBIASPC:-$COMIN_GDAS/biascr_pc.$GDUMP.$GDATE}
export GBIASAIR=${GBIASAIR:-$COMIN_GDAS/aircraft_t_bias.$GDUMP.$GDATE}
export GSATANG=${GSATANG:-$COMIN_GDAS/satang.$GDUMP.$GDATE}
export GRADSTAT=${GRADSTAT:-$COMIN_GDAS/radstat.$GDUMP.$GDATE}
# Make use of updated angle dependent bias file, if it exists.
if [[ -s $GSATANG ]]; then
   export SATANGL=$GSATANG
fi


export GSNDBF=${GSNDBF:-$COMIN_DUMP/goesnd.$CDUMP.$CDATE}
export GSNDBF1=${GSNDBF1:-$COMIN_DUMP/goesfv.$CDUMP.$CDATE}
export B1HRS2=${B1HRS2:-$COMIN_DUMP/1bhrs2.$CDUMP.$CDATE}
export B1MSU=${B1MSU:-$COMIN_DUMP/1bmsu.$CDUMP.$CDATE}
export B1HRS3=${B1HRS3:-$COMIN_DUMP/1bhrs3.$CDUMP.$CDATE}
export B1HRS4=${B1HRS4:-$COMIN_DUMP/1bhrs4.$CDUMP.$CDATE}
export B1AMUA=${B1AMUA:-$COMIN_DUMP/1bamua.$CDUMP.$CDATE}
export B1AMUB=${B1AMUB:-$COMIN_DUMP/1bamub.$CDUMP.$CDATE}
export B1MHS=${B1MHS:-$COMIN_DUMP/1bmhs.$CDUMP.$CDATE}
export B1SSU=${B1SSU:-$COMIN_DUMP/1bssu.$CDUMP.$CDATE}
export SBUVBF=${SBUVBF:-$COMIN_DUMP/osbuv8.$CDUMP.$CDATE}
export SMIPCP=/dev/null
export TMIPCP=/dev/null
export AIRSBF=${AIRSBF:-$COMIN_DUMP/airsev.$CDUMP.$CDATE}
export IASIBF=${IASIBF:-$COMIN_DUMP/mtiasi.$CDUMP.$CDATE}
export SSMITBF=${SSMITBF:-$COMIN_DUMP/ssmit.$CDUMP.$CDATE}
export SSMISBF=${SSMISBF:-$COMIN_DUMP/ssmisu.$CDUMP.$CDATE}
export GPSROBF=${GPSROBF:-$COMIN_DUMP/gpsro.$CDUMP.$CDATE}
export HRS3DB=${HRS3DB:-$COMIN_DUMP/hrs3db.$CDUMP.$CDATE}
export ESHRS3=${ESHRS3:-$COMIN_DUMP/eshrs3.$CDUMP.$CDATE}
export AMUADB=${AMUADB:-$COMIN_DUMP/amuadb.$CDUMP.$CDATE}
export ESAMUA=${ESAMUA:-$COMIN_DUMP/esamua.$CDUMP.$CDATE}
export AMUBDB=${AMUBDB:-$COMIN_DUMP/amubdb.$CDUMP.$CDATE}
export ESAMUB=${ESAMUB:-$COMIN_DUMP/esamub.$CDUMP.$CDATE}
export ESIASI=${ESIASI:-$COMIN_DUMP/esiasi.$CDUMP.$CDATE}
export IASIDB=${IASIDB:-$COMIN_DUMP/iasidb.$CDUMP.$CDATE}
export MHSDB=${MHSDB:-$COMIN_DUMP/mhsdb.$CDUMP.$CDATE}
export ESMHS=${ESMHS:-$COMIN_DUMP/esmhs.$CDUMP.$CDATE}
export SEVIRIBF=${SEVIRIBF:-$COMIN_DUMP/sevcsr.$CDUMP.$CDATE}
export ATMSBF=${ATMSBF:-$COMIN_DUMP/atms.$CDUMP.$CDATE}
export ESATMS=${ESATMS:-$COMIN_DUMP/esatms.$CDUMP.$CDATE}
export ATMSDB=${ATMSDB:-$COMIN_DUMP/atmsdb.$CDUMP.$CDATE}
export CRISBF=${CRISBF:-$COMIN_DUMP/cris.$CDUMP.$CDATE}
export CRISDB=${CRISDB:-$COMIN_DUMP/crisdb.$CDUMP.$CDATE}
export CRISFSBF=${CRISFSBF:-$COMIN_DUMP/crisfs.$CDUMP.$CDATE}
export ESCRISFS=${ESCRISFS:-$COMIN_DUMP/escrisfs.$CDUMP.$CDATE}
export CRISFSDB=${CRISFSDB:-$COMIN_DUMP/crisfsdb.$CDUMP.$CDATE}
export GOMEBF=${GOMEBF:-$COMIN_DUMP/gome.$CDUMP.$CDATE}
export OMIBF=${OMIBF:-$COMIN_DUMP/omi.$CDUMP.$CDATE}
export MLSBF=${MLSBF:-$COMIN_DUMP/mls.$CDUMP.$CDATE}
export TCVITL=${TCVITL:-$COMIN_DUMP/tcvitl.$CDUMP.$CDATE}
export STATUS=${STATUS:-$COMIN_DUMP/stat01.$CDUMP.$CDATE}
export SATWND=${SATWND:-$COMIN_DUMP/satwnd.$CDUMP.$CDATE}
export OSCATBF=${OSCATBF:-$COMIN_DUMP/oscatw.$CDUMP.$CDATE}
export B1AVHAM=${B1AVHAM:-$COMIN_DUMP/avcsam.$CDUMP.$CDATE}
export B1AVHPM=${B1AVHPM:-$COMIN_DUMP/avcspm.$CDUMP.$CDATE}
export AMSREBF=${AMSREBF:-$COMIN_DUMP/amsre.$CDUMP.$CDATE}
export GMI1CRBF=${GMI1CRBF:-$COMIN_DUMP/gmi1cr.$CDUMP.$CDATE}
export SAPHIRBF=${SAPHIRBF:-$COMIN_DUMP/saphir.$CDUMP.$CDATE}
export SFCSHPBF=${SFCSHP:-$COMIN_DUMP/sfcshp.$CDUMP.$CDATE}
export TESACBF=${TESACBF:-$COMIN_DUMP/tesac.$CDUMP.$CDATE}
export BATHYBF=${BATHYBF:-$COMIN_DUMP/bathy.$CDUMP.$CDATE}
export TRKOBBF=${TRKOBBF:-$COMIN_DUMP/trkob.$CDUMP.$CDATE}
export NSSTBF=${NSSTBF:-$COMIN_GDAS/nsstbufr.$CDUMP.$CDATE}
export PREPQC=${PREPQC:-$COMIN_GDAS/prepqc.$CDUMP.$CDATE}
export PREPQCPF=${PREPQCPF:-$COMIN_GDAS/prepbufr.acft_profiles.$CDUMP.$CDATE}

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
# Set namelist variables and input data
export RUN_SELECT=YES
export USE_SELECT=NO
export SETUP_ENKF=${SETUP_ENKF:-""}
export BKGVERR_ENKF=${BKGVERR_ENKF:-""}
export STRONGOPTS_ENKF=${STRONGOPTS_ENKF:-""}
export OBSQC_ENKF=${OBSQC_ENKF:-""}
export OBSINPUT_ENKF=${OBSINPUT_ENKF:-""}

export SETUP="miter=0,niter=1,lread_obs_save=.true.,lread_obs_skip=.false.,lwrite_predterms=.true.,lwrite_peakwt=.true.,reduce_diag=.true.,passive_bc=.false.,newpc4pred=.true.,adp_anglebc=.true.,angord=4,use_edges=.false.,diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,$SETUP_ENKF"
export BKGVERR="bkgv_flowdep=.false.,$BKGVERR_ENKF"
export STRONGOPTS="tlnmc_option=0,nstrong=0,nvmodes_keep=0,baldiag_full=.false.,baldiag_inc=.false.,$STRONGOPTS_ENKF"
export OBSQC="tcp_width=60.0,tcp_ermin=2.0,tcp_ermax=12.0,$OBSQC_ENKF"
export OBSINPUT="dmesh(1)=225.0,dmesh(2)=225.0,$OBSINPUT_ENKF"

export SFCG03=$COMIN_ENKF/bfg_${GDATE}_fhr03_ensmean
export SFCG04=$COMIN_ENKF/bfg_${GDATE}_fhr04_ensmean
export SFCG05=$COMIN_ENKF/bfg_${GDATE}_fhr05_ensmean
export SFCG06=$COMIN_ENKF/bfg_${GDATE}_fhr06_ensmean
export SFCG07=$COMIN_ENKF/bfg_${GDATE}_fhr07_ensmean
export SFCG08=$COMIN_ENKF/bfg_${GDATE}_fhr08_ensmean
export SFCG09=$COMIN_ENKF/bfg_${GDATE}_fhr09_ensmean
export SFCGES=$COMIN_ENKF/bfg_${GDATE}_fhr06_ensmean

export NSTG03=$COMIN_ENKF/nfg_${GDATE}_fhr03_ensmean
export NSTG04=$COMIN_ENKF/nfg_${GDATE}_fhr04_ensmean
export NSTG05=$COMIN_ENKF/nfg_${GDATE}_fhr05_ensmean
export NSTG06=$COMIN_ENKF/nfg_${GDATE}_fhr06_ensmean
export NSTG07=$COMIN_ENKF/nfg_${GDATE}_fhr07_ensmean
export NSTG08=$COMIN_ENKF/nfg_${GDATE}_fhr08_ensmean
export NSTG09=$COMIN_ENKF/nfg_${GDATE}_fhr09_ensmean
export NSTGES=$COMIN_ENKF/nfg_${GDATE}_fhr06_ensmean

export SIGG03=$COMIN_ENKF/sfg_${GDATE}_fhr03_ensmean
export SIGG04=$COMIN_ENKF/sfg_${GDATE}_fhr04_ensmean
export SIGG05=$COMIN_ENKF/sfg_${GDATE}_fhr05_ensmean
export SIGG06=$COMIN_ENKF/sfg_${GDATE}_fhr06_ensmean
export SIGG07=$COMIN_ENKF/sfg_${GDATE}_fhr07_ensmean
export SIGG08=$COMIN_ENKF/sfg_${GDATE}_fhr08_ensmean
export SIGG09=$COMIN_ENKF/sfg_${GDATE}_fhr09_ensmean
export SIGGES=$COMIN_ENKF/sfg_${GDATE}_fhr06_ensmean

export DIAG_SUFFIX="_ensmean"
export DIAG_COMPRESS=NO
export DIAG_TARBALL=YES
export DOHYBVAR=NO
export HYBRID_ENSEMBLE=" "


################################################################################
# Set output data

export SFCANL=$COMOUT_ENKF/sfcanl_${CDATE}_ensmean
export NSTANL=$COMOUT_ENKF/nstanl_${CDATE}_ensmean
export SFCGCY=$COMOUT_ENKF/sfcgcy_${CDATE}_ensmean
export ABIASe=${ABIASe:-$COMOUT_ENKF/biascr_int_${CDATE}_ensmean}
export GSISTAT=$COMOUT_ENKF/gsistat_${CDATE}_ensmean
export RADSTAT=$COMOUT_ENKF/radstat_${CDATE}_ensmean
export OZNSTAT=$COMOUT_ENKF/oznstat_${CDATE}_ensmean
export PCPSTAT=$COMOUT_ENKF/pcpstat_${CDATE}_ensmean
export CNVSTAT=$COMOUT_ENKF/cnvstat_${CDATE}_ensmean
export SELECT_OBS=$COMOUT_ENKF/obsinput_${CDATE}_ensmean
if [ $DOIAU = YES ]; then
  export SFCA03=$COMOUT_ENKF/sfca03_${CDATE}_ensmean
  export NSTA03=$COMOUT_ENKF/nsta03_${CDATE}_ensmean
fi
################################################################################
# Select data based on ensemble mean

$ANALYSISSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi


################################################################################
# Copy out restart and output files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

##$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST


################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND
