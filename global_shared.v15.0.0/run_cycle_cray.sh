#!/bin/sh

#BSUB -oo log.cycle
#BSUB -eo log.cycle
#BSUB -q dev
#BSUB -P GDAS-T2O
#BSUB -W 00:05
#BSUB -J gl_cycle
#BSUB -M 2400
#BSUB -W 00:02
#BSUB -extsched 'CRAYLINUX[]'

module load PrgEnv-intel cfp-intel-sandybridge/1.1.0
module list
export NODES=1
export KMP_AFFINITY=disabled
export OMP_NUM_THREADS_CY=24
export machine=WCOSS_C
export APRUNCY="aprun -n 1 -N 1 -j 1 -d $OMP_NUM_THREADS_CY -cc depth"

export global_shared_ver="v15.0.0"
export BASEDIR=/gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs/branches/cycle_fv3
export HOMEglobal=$BASEDIR/global_shared.${global_shared_ver}

export CYCLEXEC=$HOMEglobal/exec/global_cycle

export CDATE=2016100118
export FHOUR=00

TILE=tile6
export SFCGES=/gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs/branches/cycle_fv3/RESTART/20161001.180000.sfc_data.${TILE}.nc
export SFCANL="./sfcanl.$TILE"

# run without nsst
#export NST_ANL=.false.
# run with nsst
export NST_ANL=.true.
export GSI_FILE=/gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs/branches/cycle_fv3/RESTART/gdas.t18z.dtfanl

export FNTSFA=/gpfs/tp1/emc/globaldump/$CDATE/gdas/rtgssthr.grb.gdas.$CDATE
export FNSNOA=' '
export FNACNA=/gpfs/tp1/emc/globaldump/$CDATE/gdas/seaice.5min.blend.grb.gdas.$CDATE

export IALB=1
export JCAP=1534
LONB=3072
LATB=1536
export CRES=384
export FNGRID=/gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs/branches/cycle_fv3/global_shared.v15.0.0/fix/fix_fv3/C${CRES}/C${CRES}_grid.${TILE}.nc
export FNOROG=/gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs/branches/cycle_fv3/global_shared.v15.0.0/fix/fix_fv3/C${CRES}/C${CRES}_oro_data.${TILE}.nc

export FIXgsm=/gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs/branches/cycle_fv3/global_shared.v15.0.0/fix/fix_am
export FNABSC=$FIXgsm/global_mxsnoalb.uariz.t$JCAP.$LONB.$LATB.rg.grb
export FNALBC=$FIXgsm/global_snowfree_albedo.bosu.t$JCAP.$LONB.$LATB.rg.grb
export FNVETC=$FIXgsm/global_vegtype.igbp.t$JCAP.$LONB.$LATB.rg.grb
export FNSOTC=$FIXgsm/global_soiltype.statsgo.t$JCAP.$LONB.$LATB.rg.grb
export FNZORC="igbp"
export FNALBC2=$FIXgsm/global_albedo4.1x1.grb
export FNTSFC=$FIXgsm/RTGSST.1982.2012.monthly.clim.grb
export FNAISC=$FIXgsm/CFSR.SEAICE.1982.2012.monthly.clim.grb
export FNSMCC=$FIXgsm/global_soilmgldas.t$JCAP.$LONB.$LATB.grb

export NWPROD=/nwprod

export FILESTYLE=L
export use_ufo=.true.
export VERBOSE=YES
# turn off sst update to isolate nsst changes
export CYCLVARS=FSNOL=99999.,FSNOS=99999.,FTSFS=99999.,
export ISOT=1
export IVEGSRC=1
export JCAP=1534

export DATA=/gpfs/hps3/stmp/George.Gayno/global_cycle
rm -fr $DATA

$HOMEglobal/ush/global_cycle.sh $SFCGES $SFCANL

exit
