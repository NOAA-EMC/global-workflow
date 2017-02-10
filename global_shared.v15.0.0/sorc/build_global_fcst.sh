#!/bin/ksh
set -x

curdir=`pwd`

. $MODULESHOME/init/ksh

cd $curdir/global_fcst.fd/NEMS/src
./configure gsm_intel_wcoss_c

source conf/modules.nems                         
module list                                     
gmake clean                                    
gmake gsm J=-j2                                    

cp ../exe/NEMS.x $curdir/../exec/global_fcst

