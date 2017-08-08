#!/bin/sh

#BSUB -oo log.cycle
#BSUB -eo log.cycle
#BSUB -q dev
#BSUB -a poe
#BSUB -n 1
#BSUB -x
#BSUB -P GDAS-T2O
#BSUB -R span[ptile=1]
#BSUB -R affinity[cpu(32)]
#BSUB -W 00:05
#BSUB -J gl_cycle

set -x

export global_shared_ver="v15.0.0"
export BASEDIR=/global/save/George.Gayno/fv3gfs/branches/cycle_fv3
export HOMEglobal=$BASEDIR/global_shared.${global_shared_ver}

export CYCLEXEC=$HOMEglobal/exec/global_cycle

export CDATE=2016092900
export FHOUR=00

export SFCGES=/ptmpp1/George.Gayno/control/C384/gfs.20160929/00/RESTART/sfc_data.tile1.nc
export SFCANL="./sfc_data.tile1.nc"

export FNTSFA=/globaldump/$CDATE/gdas/rtgssthr.grb.gdas.$CDATE
export FNSNOA=' '
export FNACNA=/globaldump/$CDATE/gdas/seaice.5min.blend.grb.gdas.$CDATE

export JCAP=1534
export LONB=3072
export LATB=1536
export CRES=384

export FNGRID=/global/save/George.Gayno/fv3gfs/branches/cycle_fv3/global_shared.v15.0.0/fix/fix_fv3/C${CRES}/C${CRES}_grid.tile1.nc
export FNOROG=/global/save/George.Gayno/fv3gfs/branches/cycle_fv3/global_shared.v15.0.0/fix/fix_fv3/C${CRES}/C${CRES}_oro_data.tile1.nc

export FIXgsm=/global/save/George.Gayno/fv3gfs/branches/cycle_fv3/global_shared.v15.0.0/fix/fix_am

export KMP_STACKSIZE=2048m
export OMP_NUM_THREADS_CY=32
export NTHSTACK=1024000000
export machine=WCOSS

export NWPROD=/nwprod

export FILESTYLE=L
export VERBOSE=YES
export CYCLVARS=FSNOL=99999.,FSNOS=99999.,

export VERBOSE=YES

export DATA=/stmpp1/George.Gayno/global_cycle
rm -fr $DATA

$HOMEglobal/ush/global_cycle.sh $SFCGES $SFCANL

exit
