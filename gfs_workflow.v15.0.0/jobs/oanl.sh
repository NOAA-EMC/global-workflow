#!/bin/ksh
################################################################################
# This script runs the global ocean analysis.
# Usage: oanl.sh
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
#   GODASSH
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
#
export BASEDIR=${BASEDIR:-..}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export FIXDIR=${FIXDIR:-$BASEDIR/fix/fix_am}
export FIXgsm=${FIXgsm:-$FIXDIR}
export FIX_OCN=${FIX_OCN:-$BASEDIR/fix/fix_om}
export PARMOCN=${PARMOCN:-$BASEDIR/parm/parm_om}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}

$PBEG

################################################################################
# Set other variables

export cfsd=${cfsd:-cfs_cdas_}
export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export WGRIB=${WGRIB:-$NWPROD/util/exec/wgrib}
export GODASSH=${GODASSH:-$SCRDIR/excfs_cdas_godas.sh.sms}
export OPREPSH=${OPREPSH:-$USHDIR/${cfsd}oceanprep.sh}

#export GODASSH=${GODASSH:-$USHDIR/godasM4.sh}
#export OPREPSH=${OPREPSH:-$USHDIR/oprep_job.sh}
#
export MP_SHARED_MEMORY=yes
export MP_LABELIO=yes
export MP_COREFILE_FORMAT=lite
export VERBOSE=YES
export CDFNL=${CDFNL:-gdas}
COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
export GDUMP=${GDUMP:-$CDFNL}
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
export gdas_cyc=${gdas_cyc:-4}
export Obs_dir=${Obs_dir:-$COMROT/GDS_Obs}
export SBC_dir=${SBC_dir:-$COMROT/GDS_SBC}
export RUN_OPREP=${RUN_OPREP:-NO}
export RUN_RTDUMP=${RUN_RTDUMP:-YES}
export cWGsh=${cWGsh:-$USHDIR/${cfsd}cWG.sh}
#export cWGsh=${cWGsh:-$USHDIR/cWG.sh}
#
nknd=${CKND:-1}
export JCAP=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$nknd -d,)
export omres=$(eval echo \${OMRESFCST$cycle$cdump:-${omres:-1x1}}|cut -f$nknd -d,)
#export omres=${omres:-1x1}
export GRIDSPEC=$FIX_OCN/grid_spec_$omres.nc.T$JCAP
export chl=$FIX_OM/chl_$omres.nc
export salt_sfc_restore=$FIX_OM/salt_sfc_restore_$omres.nc
export temp_sfc_restore=$FIX_OM/temp_sfc_restore_$omres.nc
export CHLNC=$FIX_OCN/chl_$omres.nc
export RUNOFFNC=$FIX_OCN/runoff_$omres.nc
export SALTSFCRESTORE=$FIX_OCN/salt_sfc_restore_$omres.nc
export namelist=$PARM_GODAS/namelist_$omres # namelist file
export ASYM_GODAS=${ASYM_GODAS:-NO}
export GODAS_WNDO=${GODAS_WNDO:-14}
export lores=${lores:-""}       # For T126 copupled analysis set lores=_lores
if [ $ASYM_GODAS = YES ] ; then
  if [ $GODAS_WNDO -eq 10 -o $GODAS_WNDO -eq 14 ] ; then
    export GODAS_DATA_DELAY=${GODAS_DATA_DELAY:-""}
    export namelist=$PARM_GODAS/namelist_${omres}_asym_${GODAS_WNDO}$lores
  else
    echo ' THIS ASYMMETRIC GODAS WINDOW NOT SUPPORTED AT THIS TIME'
    exit 333
  fi
fi
#
if [ $omres = '1x1' ] ; then
 export dt_cpld=${dt_1x1:-3600}                      # OM coupling time interval
 export dt_ocean=${dt_1x1:-3600}                     # OM time step
 export im_mom4=360
 export jm_mom4=231
 export jmtp_mom4=25
 export imos=80
else
 export dt_cpld=${dt_05:-1800}                       # OM coupling time interval
 export dt_ocean=${dt_05:-1800}                      # OM time step
 export im_mom4=720
 export jm_mom4=410
 export jmtp_mom4=50
 export imos=160
fi
#
$PCOP $CDATE/$CDUMP/$CSTEP/DMPI $COMDMP $DATA <$RLIST

if [ $RUN_OPREP = YES ] ; then                  # Run ocean prep step here
 for cdm in $(eval echo $COMDMP|tr , ' ') ; do
  if [ -s $cdm/cbathy* ] ; then
    nn=$(echo $cdm | tr / ' ' | wc -w)
    xx=$(echo $cdm | tr / ' ' | eval awk \'{print \$$nn}\')
    nc=$(echo $xx | wc -c)
    if [ $xx != $CDUMP ] ; then
      export DMP_SUF=$(echo $xx | cut -c$((nc-1)))
    else
      export DMP_SUF=""
    fi
    nt=$(echo $cdm | wc  -c)
    nd=$(echo $CDATE | wc  -c)
    export cdumpdir=$(echo $cdm | cut -c -$((nt-nc-nd-1)))
    break
  fi
 done
 mkdir -p $DATA/ocn_prep
 cd $DATA/ocn_prep
 $OPREPSH ocn_prep $LOGNAME $CDATE $CDUMP $cdumpdir
 export ocn_dump=$DATA/ocn_prep
 cd $DATA
else
 for cdm in $(eval echo $COMDMP|tr , ' ') ; do
  if [ -s $cdm/tmp.prf* ] ; then
    nn=$(echo $cdm | tr / ' ' | wc -w)
    xx=$(echo $cdm | tr / ' ' | eval awk \'{print \$$nn}\')
    nc=$(echo $xx | wc -c)
    if [ $xx != $CDUMP ] ; then
      nc=$(echo $xx | wc -c)
      export DMP_SUF=$(echo $xx | cut -c$((nc-1)))
    else
      export DMP_SUF=""
    fi
    nt=$(echo $cdm | wc  -c)
    nd=$(echo $CDATE | wc  -c)
    export cdumpdir=$(echo $cdm | cut -c -$((nt-nc-nd-1)))
    break
  fi
 done
fi
################################################################################
# Run godas analysis

#$GODASSH $CDATE $CDUMP $COMROT $RESDIR $FIX_OCN $DATA $Obs_dir $SBC_dir
$GODASSH $CDATE $CDUMP $COMROT $RESDIR $FIX_OCN $DATA $Obs_dir
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
#

################################################################################
# Copy out restart and output files

#$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
rc=$?

#$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
