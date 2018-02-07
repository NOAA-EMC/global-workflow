#!/bin/ksh
set -x

curdir=`pwd`

. $MODULESHOME/init/ksh

cd $curdir/global_fcst.fd/NEMS/src
./configure gsm_intel_wcoss_c

source ./conf/modules.nems.sh
module list                                     

export COMP=GSM
export COMP_SRCDIR=$curdir/global_fcst.fd/GSM
export COMP_BINDIR=$curdir/global_fcst.fd/GSM-INSTALL
export GSM_BUILDOPT="GOCART_MODE=stub"
mkdir -p $COMP_BINDIR
cd $COMP_SRCDIR
./configure gsm_intel_wcoss_c
gmake GSM_DIR=$COMP_SRCDIR nuopcdistclean                                    
gmake GSM_DIR=$COMP_SRCDIR $GSM_BUILDOPT DESTDIR= INSTDIR=$COMP_BINDIR nuopcinstall
cd  $curdir/global_fcst.fd/NEMS/src
make nems COMP=,gsm, GSM_DIR=$COMP_BINDIR

cp ../exe/NEMS.x $curdir/../exec/global_fcst

