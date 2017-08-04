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

export global_shared_ver="v15.0.0"
export BASEDIR=/global/save/George.Gayno/fv3gfs/branches/cycle_fv3
export HOMEglobal=$BASEDIR/global_shared.${global_shared_ver}

export CYCLEXEC=$HOMEglobal/exec/global_cycle

export CDATE=2015121300
export FHOUR=00

export SFCGES=/global/noscrub/George.Gayno/gfs_soil_veg_alb_fy17/sfnanl.gfs.${CDATE}.new.alb
export SFCANL="./gdas1.t00z.sfcanl"

export FNTSFA=/globaldump/$CDATE/gdas/rtgssthr.grb.gdas.$CDATE
export FNSNOA=' '
export FNACNA=/globaldump/$CDATE/gdas/seaice.5min.blend.grb.gdas.$CDATE

export IALB=1
export JCAP=1534
LONB=3072
LATB=1536
export CRES=384

export FIXgsm=/global/save/George.Gayno/fv3gfs/branches/cycle_fv3/global_shared.v15.0.0/fix/fix_am
export FNABSC=$FIXgsm/global_mxsnoalb.uariz.t$JCAP.$LONB.$LATB.rg.grb
export FNALBC=$FIXgsm/global_snowfree_albedo.bosu.t$JCAP.$LONB.$LATB.rg.grb
export FNVETC=$FIXgsm/global_vegtype.igbp.t$JCAP.$LONB.$LATB.rg.grb
export FNSOTC=$FIXgsm/global_soiltype.statsgo.t$JCAP.$LONB.$LATB.rg.grb
export FNZORC="igbp"
export FNALBC2=$FIXgsm/global_albedo4.1x1.grb
export FNMASK=$FIXgsm/global_slmask.t$JCAP.$LONB.$LATB.rg.grb
export FNOROG=$FIXgsm/global_orography.t$JCAP.$LONB.$LATB.rg.grb
export FNOROG_UF=$FIXgsm/global_orography_uf.t$JCAP.$LONB.$LATB.rg.grb
export FNTSFC=$FIXgsm/RTGSST.1982.2012.monthly.clim.grb
export FNAISC=$FIXgsm/CFSR.SEAICE.1982.2012.monthly.clim.grb

export KMP_STACKSIZE=2048m
export OMP_NUM_THREADS_CY=32
export NTHSTACK=1024000000
export machine=WCOSS

export NWPROD=/nwprod

export FILESTYLE=L
export use_ufo=.true.
export VERBOSE=YES
export CYCLVARS=FSNOL=99999.,FSNOS=99999.,
export ISOT=1
export IVEGSRC=1
export JCAP=1534

export DATA=/stmpp1/George.Gayno/global_cycle
rm -fr $DATA

$HOMEglobal/ush/global_cycle.sh $SFCGES $SFCANL

exit
