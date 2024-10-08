#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

member=$1
export SIGINP=$2
export SIGOUT=$3
export JCAP=$4
export LATB=$5
export LONB=$6

DATATMP=$DATA/$member
mkdir -p $DATATMP
cd $DATATMP

export LEVS=${LEVS_LORES:-64}

export CHGRESSH=${CHGRESSH:-${USHgfs}/global_chgres.sh}
export CHGRESEXEC=${CHGRESEXEC-${EXECgfs}/global_chgres}
export OROGRAPHY=${OROGRAPHY_LORES:-${FIXgfs}/am/global_orography.t$JCAP.$LONB.$LATB.grb}
export OROGRAPHY_UF=${OROGRAPHY_UF_LORES:-${FIXgfs}/am/global_orography_uf.t$JCAP.$LONB.$LATB.grb}
export LONSPERLAT=${LONSPERLAT_LORES:-${FIXgfs}/am/global_lonsperlat.t${JCAP}.$LONB.$LATB.txt}
export SLMASK=${SLMASK_LORES:-${FIXgfs}/am/global_slmask.t$JCAP.$LONB.$LATB.grb}
export MTNVAR=${MTNVAR_LORES:-${FIXgfs}/am/global_mtnvar.t$JCAP.$LONB.$LATB.f77}
export SIGLEVEL=${SIGLEVEL_LORES:-${FIXgfs}/am/global_hyblev.l${LEVS}.txt}
export O3CLIM=${O3CLIM:-${FIXgfs}/am/global_o3clim.txt}

use_ufo=.true.

NTRAC=3
IALB=0
idvc_a=2
idvt=21
IDSL=1
IDVM=0
LATCH=8
OUTTYP=2
export CHGRESTHREAD=${CHGRESTHREAD_LORES:-2}
export CHGRESVARS="use_ufo=$use_ufo,IALB=$ialb,ntrac=$NTRAC,idvc=$idvc_a,idvt=$idvt,idsl=$IDSL,IDVM=$IDVM,OUTTYP=$OUTTYP,"

export DATA=$DATATMP

export APRUNC=${APRUNC:-""}
export VERBOSE=YES

echo "execute $CHGRESSH for $member"
$CHGRESSH
rc=$?

export ERR=$rc
export err=$ERR

exit $err

