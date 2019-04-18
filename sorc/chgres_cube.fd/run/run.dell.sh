#!/bin/bash

#BSUB -oo log
#BSUB -eo log
#BSUB -q debug
#BSUB -P FV3GFS-T2O
#BSUB -J chgres.fv3
#BSUB -W 0:15
#BSUB -x                 # run not shared
#BSUB -n 6               # total tasks
#BSUB -R span[ptile=6]   # tasks per node
#BSUB -R affinity[core(4):distribute=balance]

set -x

module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load impi/18.0.1
module load lsf/10.1
module use /usrx/local/dev/modulefiles
module load NetCDF/4.5.0

# On Dell, always set environment variables AFTER any module loads.
# Threads are only useful when using gfs sigio files.  Set OMP_NUM_THREADS
# equal to "affinity[core(n)".

export OMP_STACKSIZE=1024M
export OMP_NUM_THREADS=4

EXECDIR=/gpfs/dell2/emc/modeling/noscrub/George.Gayno/fv3gfs.git/global-workflow/chgres_cube/exec
RUNDIR=/gpfs/dell2/emc/modeling/noscrub/George.Gayno/fv3gfs.git/global-workflow/chgres_cube/run

WORKDIR=/gpfs/dell1/stmp/George.Gayno/chgres.fv3
rm -fr $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

#cp $RUNDIR/config.C1152.l91.dell.nml ./fort.41
#cp $RUNDIR/config.C768.l91.dell.nml ./fort.41
#cp $RUNDIR/config.C384.dell.nml ./fort.41
#cp $RUNDIR/config.C384.gaussian.dell.nml ./fort.41
#cp $RUNDIR/config.C48.dell.nml ./fort.41
cp $RUNDIR/config.C48.gfs.spectral.dell.nml  ./fort.41

mpirun $EXECDIR/global_chgres.exe

exit
