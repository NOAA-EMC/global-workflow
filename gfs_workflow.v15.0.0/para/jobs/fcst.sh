#!/bin/ksh
set -ux

################################################################################
# This script runs the forecast.
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
################################################################################

set -a;. $CONFIG;set +a
echo "-----end of $CONFIG ------------"
echo

export CKSH=${CKSH:-$(echo $CSTEP|cut -c-4)}
export CKND=${CKND:-$(echo $CSTEP|cut -c5-)}
export machine=${machine:-WCOSS_C}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
rm -rf $DATA||exit 1; mkdir -p $DATA||exit 1; cd $DATA||exit 1
chmod ${permission:-755} $DATA

#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
export VERBOSE=YES
################################################################################


$FORECASTSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND

