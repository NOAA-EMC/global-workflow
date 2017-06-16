#!/bin/ksh
################################################################################
# This script copies restart files.
# Usage: copy.sh
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
export CKND2=$(echo $CKND|cut -c5-)
eval export DATA=$DATATMP
eval export COMCOP=$COMCOP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-..}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export FIXDIR=${FIXDIR:-$BASEDIR/fix/fix_am}
export FIXgsm=${FIXgsm:-$FIXDIR}
export NWPROD=${NWPROD:-BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
export SATZEROSH=${SATZEROSH:-$USHDIR/satbias_zero.sh}
export SATINFO=${SATINFO:-$FIXgsm/global_satinfo.txt}
export SET_BIASCR=${SET_BIASCR:-NO}
export SET_SATANG=${SET_SATANG:-NO}
export SET_FIX_FLDS=${SET_FIX_FLDS:-NO}
export SFCFIXFLDSSH=${SFCFIXFLDSSH:-$USHDIR/global_sfc_fix_flds.sh}
export SIGHDR=${SIGHDR:-$NWPROD/exec/global_sighdr}
export COUP_FCST=${COUP_FCST:-NO}
$PBEG

################################################################################
# Set other variables

export CKND=${CKND:-fcst1}
export CSTEP=$CKSH$CKND
#export PMKR=${PMKR:-$SHDIR/pmkr}
export PCOP=${PCOP:-$SHDIR/pcop}
export COPYCH=${COPYCH:-YES}
export XC=${XC:-""}
export CHGRESSH=${CHGRESSH:-$NWPROD/ush/global_chgres.sh}
export CYCLESH=${CYCLESH:-$NWPROD/ush/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-$$NWPROD/exec/global_cycle$XC}
#
if [ $SET_FIX_FLDS = YES ] ; then
 export SAVEDIR=${SAVEDIR:-${MODISDIR:-$PTMP/$LOGNAME/sfc_fix_flds}}
 export MTNDIR=${MTNDIR:-$SAVEDIR}
 $SFCFIXFLDSSH $SAVEDIR $fseg $MTNDIR
fi

################################################################################
# Make Rlist if it's missing
#
#$PMKR >$RLIST
#[[ -s $RLIST ]]||$PMKR >$RLIST
#
################################################################################
# Copy in restart and input files

$PCOP $CDATE/$CDUMP/$CKND/COPI $COMCOP $DATA <$RLIST
#
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

if [ $COUP_FCST = YES ] ; then
 ${NCP:-/bin/cp} -p $COMCOP/ocnanl.$CDUMP.$CDATE.tar $COMROT/
 ${NCP:-/bin/cp} -p $COMCOP/noah.rst.$CDUMP.$CDATE   $COMROT/
fi

#
#  Copy initial ABIAS and SATANF files
#
#export PREINP=gdas1.t$(echo $CDATE|cut -c9-10)z.
#$NCP $SHDIR/${PREINP}abias $DATA/${PREINP}abias
#$NCP $SHDIR/${PREINP}satang $DATA/${PREINP}satang
################################################################################
# Change resolution if necessary

if [[ $CKND = fcst* ]];then
  if [[ $COPYCH = YES ]];then
    export SIGINP=sighir.$CDUMP.$CDATE
    export SFCINP=sfchir.$CDUMP.$CDATE
    export SIGOUT=$SIGISUF.$CDUMP.$CDATE
    export SFCOUT=$SFCISUF.$CDUMP.$CDATE
    if [ $CDATE -ne $($SIGHDR $SIGINP idate) ] ; then  
      $NWPROD/util/ush/overdate.sigma.sh $CDATE $SIGOUT $SIGINP
    else
      mv $SIGOUT $SIGINP
    fi
    mv $SFCOUT $SFCINP
#   export LANDICE_OPT=1
    cycle=$(echo $CDATE|cut -c9-10)
    cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
    nknd=${CKND2:-1}
    export IDVC=$(eval echo \${IDVCFCST$cycle$cdump:-$idvc_a}|cut -f$nknd -d,)
    export LANDICE_OPT=$(eval echo \${LICEFCST$cycle$cdump:-2}|cut -f$nknd -d,)
#   export IDVC=$(eval echo \${IDVCFCST$cycle$cdump:-$idvc_a})
#   export LANDICE_OPT=$(eval echo \${LICEFCST$cycle$cdump:-2})
    export LATCH=${LATCH:-48}
    export ivssfc=${ivssfc:-200509}
    export ivssig=${ivssig:-198410}
    export CHGRESVARS=${CHGRESVARS:-""}
    if [ $IDVC = 1 ] ; then
      export IDVM=1 ; export IDSL=1 ; export nvcoord=1
    elif [ $IDVC = 2 ] ; then
      export IDVM=1 ; export IDSL=1 ; export nvcoord=2
      export SIGLEVEL=${SIGLEVEL:-$FIXgsm/global_hyblev.l$LEVS.txt}
    elif [ $IDVC = 3 ] ; then
# export IDVM=2 ; export IDSL=2 ; export nvcoord=3
      Apercent=${Apercent:-050} ; export IDSL=2
      if [ $Apercent -lt 100 ] ; then
        export SIGLEVEL=${SIGLEVEL:-$FIXgsm/global_hyblev3.ipa$Apercent.l$LEVS.txt}
      else
        export SIGLEVEL=${SIGLEVEL:-$FIXgsm/global_hyblev.l$LEVS.txt}
      fi

#
#     SFCPRESS_ID=0 or 1 for ln(psfc), 2 for psfc
#     THERMODYN_ID=3 for enthalphy, 0 or 1 for virtual T, 2 for T
#
      if [ $ENTHALPY = YES ] ; then
        export SFCPRESS_ID=2
        export THERMODYN_ID=3
#
#***************************************************************
#                                N2 ,     H2O,     O3,        CLW
#       export RIlist=${RIlist:-"   296.8034, 461.50, 173.2247,    0.0"}
#       export CPIlist=${CPIlist:-" 1039.645, 1846.0, 820.2391,    0.0"}
#                                 Dry Air ,  H2O,     O3,        CLW
        export RIlist=${RIlist:-"   287.05, 461.50, 173.2247,    0.0"}
        export CPIlist=${CPIlist:-" 1004.6, 1846.0, 820.2391,    0.0"}
#
        export CHGRESVARS="${CHGRESVARS}RI=$RIlist,CPI=$CPIlist,"
      else
        export SFCPRESS_ID=1
        export THERMODYN_ID=1
      fi
      export IDVM=${THERMODYN_ID}${SFCPRESS_ID}
      export nvcoord=3
    else
      echo 'Invalid IDVC = ',IDVC ; exit 111
    fi
    export XLSMPOPTS="parthds=32:spins=0:yields=0:stack=128000000"
    export CHGRESVARS="IDVC=$IDVC,IVSSFC=$ivssfc,IVSSIG=$ivssig,NVCOORD=$nvcoord,IDVM=$IDVM,IDSL=$IDSL,LATCH=$LATCH,$CHGRESVARS"

    export FNVETC=$FIXDIR/global_vegtype.1x1.grb
    export CLIMO_FIELDS_OPT=3

    $CHGRESSH
    rc=$?
    if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
#                                         Call surface cycle
    export FNTSFA=$COMCOP/sstgrb.$CDUMP.$CDATE
    export FNACNA=$COMCOP/icegrb.$CDUMP.$CDATE
    export FNSNOA=$COMCOP/snogrb.$CDUMP.$CDATE
    if [ -s $FNTSFA -a -s $FNSNOA ] ; then
      mv $SFCOUT $SFCINP
      $CYCLESH $SFCINP $SFCOUT
      if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
    fi
  fi
#elif [[ $CKND = prep* ]];then
#  echo COPY only works for $CKND without CHGRES
else
  echo COPY does not work for $CKND yet;$PERR;exit 1
fi

################################################################################
# Make output directories

mkdir -p $COMROT

################################################################################
# Copy out restart and output files

$PCOP $CDATE/$CDUMP/$CKND/COPI $DATA $COMROT <$RLIST
rc=$?

if [ $SET_BIASCR = YES -a $SET_SATANG = YES ] ; then
 $SATZEROSH $SATINFO $DATA/biassat0 $COMROT/biascr.$CDUMP.$CDATE \
                                    $COMROT/satang.$CDUMP.$CDATE
elif [ $SET_BIASCR = YES ] ; then
 $SATZEROSH $SATINFO $DATA/biassat0 $COMROT/biascr.$CDUMP.$CDATE /dev/null
elif [ $SET_SATANG = YES ] ; then
 $SATZEROSH $SATINFO $DATA/biassat0 /dev/null $COMROT/satang.$CDUMP.$CDATE
fi
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
