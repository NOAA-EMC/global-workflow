#!/bin/sh --login

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -o gfs_sminit.o%J 
#BSUB -e gfs_sminit.o%J
#BSUB -J gfs_sminit      
#BSUB -q dev
#BSUB -W 04:30 
#BSUB -M 1024
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -cwd /gpfs/hps/emc/global/noscrub/Michael.A.Young/dng_q3fy17/driver

#############################################################
#  Description:     Processes the even hours for GFS SMINIT 
#                   Required to run with companion script run_JGFS_SMINIT.odd.sh
#
#  Usage:           bsub < run_JGFS_SMINIT.even.sh
#                   bsub < run_JGFS_SMINIT.odd.sh
#
#  Input COMIN :    /gpfs/hps/ptmp/emc.glopara/com2/gfs/para/${RUN}.${PDY}
#
#  Temp Output DATA:     $DATA_IN/gfsdng_${RUNTYP}.${pid}  
#  Permanent Output : $COMOUT
#
#############################################################

set -xa

export NODES=5
export OMP_NUM_THREADS=1
export KMP_STACKSIZE=2048m

export modeld=${modeld:-smartinit}
export model_ver=gfs
export RUN=gfs
export RUNTYP=guamnest
export RUN_ENVIR=prod
export envir=prod
export smart_ver=${smart_ver:-v4.0.0}
export util_ver=1.0.5			#Phase2: v1.0.0; Cray: v1.0.5
export cyc=00
export PDY=20170228
export pid=$$
export nemsioget=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17/global_shared.v14.1.0/exec/nemsio_get

. $MODULESHOME/init/sh
module load prod_util/$util_ver
module load prod_envir
module load g2tmpl-intel/1.4.0
module load grib_util/1.0.3


export DATA_IN=/gpfs/hps/ptmp/Michael.A.Young
export DATA=$DATA_IN/gfsdng_${RUNTYP}.${pid}
export COMIN=/gpfs/hps/ptmp/emc.glopara/com2/gfs/para/${RUN}.${PDY}
####export COMIN=/gpfs/hps/ptmp/Michael.A.Young/data.smartinit.4.cycles
export COMOUT=/gpfs/hps/ptmp/Michael.A.Young/gfsdng_${RUNTYP}.${PDY}.${cyc}
export pcom=/gpfs/hps/ptmp/Michael.A.Young/pcom2/prod/gfs

export HOMEdng=/gpfs/hps/emc/global/noscrub/Michael.A.Young/dng_q3fy17
export HOMEPOST=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17_final/global_shared.v14.1.0
export HOMEutil=${HOMEutil:-/gpfs/hps/nco/ops/nw${envir}/prod_util.${util_ver}}

export POSTGPEXEC=$HOMEPOST/exec/ncep_post
export POSTGPSH=$HOMEPOST/ush/global_nceppost.sh
export SMARTINIT=$HOMEdng/exec/smartinit
export CTLFILE=$HOMEdng/parm/gfs_downscale_cntrl.parm
export PARMglobal=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17_final/global_shared.v14.1.0/parm
export FIXglobal=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17_final/global_shared.v14.1.0/fix

export APRUNP="time aprun -n 60 -N 12 -d 2 -cc depth"
export APRUN_SMARTPRECIP="aprun -q -j1 -n1 -N1 -d1 -cc depth"
export APRUN_SMARTINIT="aprun -q -j1 -n1 -N1 -d1 -cc depth"

export GFS_SMINIT_SH=$HOMEdng/ush/gfs_smartinit.sh
export SHOUR=00 
export EHOUR=192
export EHOUR_HRLY=12
export FHINC_HRLY=2
export FHINC=6


export LONB=1440
export LATB=721

if [ $LONB -eq 1440 ] ; then
  export fgrid='255 0 1440 721 90000 0 128 -90000 359750 250 250 0'
else
  export fgrid='255 0 720 361 90000 0 128 -90000 359500 500 500 0'
fi

export VERBOSE=YES
export OUTTYP=4
export IDRT=4  #output gaussian grib if 4 and lat/lon grid if 0
export MODEL_OUT_FORM=binarynemsiompiio
export NTHREADS=4
export NTHSTACK=1024000000
export LONB=3072
export LATB=1536

$HOMEdng/jobs/JGFS_SMINIT

