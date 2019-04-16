#!/bin/sh

#BSUB -oo log
#BSUB -eo log
#BSUB -q debug
#BSUB -J chgres_fv3
#BSUB -P FV3GFS-T2O
#BSUB -W 0:10
#BSUB -M 1000
#BSUB -extsched 'CRAYLINUX[]'

set -x

export NODES=1
# threads useful when using gfs sigio files as input
export OMP_NUM_THREADS=1
#export OMP_NUM_THREADS=4
export OMP_STACKSIZE=1024M

WORK_DIR=/gpfs/hps3/stmp/George.Gayno/chgres_fv3
rm -fr $WORK_DIR
mkdir -p $WORK_DIR
cd $WORK_DIR

#cp /gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs.git/global-workflow/chgres_cube/run/config.C384.cray.nml ./fort.41
#cp /gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs.git/global-workflow/chgres_cube/run/config.C768.nest.cray.nml ./fort.41
#cp /gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs.git/global-workflow/chgres_cube/run/config.C768.stretch.cray.nml ./fort.41
cp /gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs.git/global-workflow/chgres_cube/run/config.C48.cray.nml ./fort.41

EXEC_DIR=/gpfs/hps3/emc/global/noscrub/George.Gayno/fv3gfs.git/global-workflow/chgres_cube/exec

export KMP_AFFINITY=disabled
aprun -j 1 -n 6 -N 6 -d${OMP_NUM_THREADS} -cc depth $EXEC_DIR/global_chgres.exe
#aprun -j 1 -n 18 -N 18 -d${OMP_NUM_THREADS} -cc depth $EXEC_DIR/global_chgres.exe

exit
