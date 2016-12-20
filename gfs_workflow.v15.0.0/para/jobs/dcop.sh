#!/bin/ksh
################################################################################
# This script retrieves data assimilation files.
# Usage: dcop.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   RLIST
#   DATATMP
#   COMDMP
#   NCP
#   GETGDASSH
#   PBEG
#   PCOP
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
export NWPROD=${NWPROD:-BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

COMDMPTMP=${COMDMPTMP:-$COMDMP}
COMROTTMP=${COMROTTMP:-$COMROT}
eval export COMDMP=$COMDMPTMP
eval export COMROT=$COMROTTMP
export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export USHGLOBAL=${USHGLOBAL:-${USHDIR:-$BASEDIR/ush}}
export GETGDASSH=${GETGDASSH:-$USHGLOBAL/global_getgdas.sh}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-fnl}
export COMOUT=$DATA
export PREOUT=''
export SUFOUT='.$CDUMP.$CDATE'
export VERBOSE=YES

################################################################################
# Get data assimilation files
  
if [[ $CDATE = ????????00 ]]; then
if [[ $CDUMP = gfs ]];then
  $GETGDASSH $CDATE $CDUMP $SFCISUF $SIGISUF
else
  $GETGDASSH $CDATE $CDUMP biascr satang $SFCISUF $SIGISUF
fi
fi
  
################################################################################
# Copy out data assimilation files
  
mkdir -p $COMDMP
$PCOP $CDATE/$CDUMP/$CSTEP/DMPO $DATA $COMDMP <$RLIST
rc=$?

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
