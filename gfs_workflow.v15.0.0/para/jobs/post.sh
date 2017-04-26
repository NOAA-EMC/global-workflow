#!/bin/ksh
################################################################################
# This script runs the post processor.
# Usage: post.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
################################################################################
set -ux

set -a;. $CONFIG;set +a
echo "-----end of $CONFIG ------------"
echo 

export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
export machine=${machine:-WCOSS}
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
if [ $DATA != $COMROT ]; then
 rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1
fi
cd $DATA||exit 1
chmod ${permission:-755} $DATA
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG
################################################################################

if [ ${REMAP_GRID:-latlon} = latlon ]; then
 echo
 $REMAPSH                                         #remap 6-tile output to global array in netcdf
 if [[ $? -ne 0 ]];then $PERR;exit 1;fi

 echo
 $NC2NEMSIOSH                                     #convert netcdf to nemsio
 if [[ $? -ne 0 ]];then $PERR;exit 1;fi
else
 echo
 $REGRIDNEMSIOSH
 if [[ $? -ne 0 ]];then $PERR;exit 1;fi
fi

echo
$POSTJJOB                                        #converts nemsio to grib2 and run down-stream jobs
if [[ $? -ne 0 ]];then $PERR;exit 1;fi


################################################################################
# Exit gracefully
if [ ${KEEPDATA:-NO} != YES -a $DATA != $COMROT ] ; then rm -rf $DATA ; fi
$PEND

