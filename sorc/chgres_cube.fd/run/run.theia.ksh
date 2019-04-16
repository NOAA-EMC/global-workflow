#!/bin/ksh

#-----------------------------------------------------------
# Run test case on Theia.  MUST BE RUN WITH A 
# MULTIPLE OF SIX MPI TASKS.  Could not get it to
# work otherwise.
#-----------------------------------------------------------

#PBS -l nodes=2:ppn=6
#PBS -l walltime=0:10:00
#PBS -A fv3-cpu
#PBS -q debug
#PBS -N fv3
#PBS -o ./log
#PBS -e ./log

set -x

np=$PBS_NP

source /apps/lmod/lmod/init/ksh
module purge
module load intel/15.1.133
module load impi/5.1.1.109 
module load netcdf/4.3.0

# Threads are useful when processing spectal gfs data in
# sigio format.  Otherwise, use one thread.
export OMP_NUM_THREADS=1
export OMP_STACKSIZE=1024M

WORKDIR=/scratch3/NCEPDEV/stmp1/$LOGNAME/chgres_fv3
rm -fr $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

#ln -fs ${PBS_O_WORKDIR}/config.C48.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C48.gaussian.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C48.gfs.gaussian.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C48.gfs.spectral.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C384.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C768.nest.atm.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C768.nest.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C768.atm.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C768.l91.atm.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C1152.l91.atm.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C96.nest.theia.nml ./fort.41
ln -fs ${PBS_O_WORKDIR}/config.C768.stretch.theia.nml ./fort.41
#ln -fs ${PBS_O_WORKDIR}/config.C1152.theia.nml ./fort.41

mpirun -np $np ${PBS_O_WORKDIR}/../exec/global_chgres.exe

exit 0
