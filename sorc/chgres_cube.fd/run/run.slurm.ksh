#!/bin/ksh

#-----------------------------------------------------------
# Run test case on Theia.  MUST BE RUN WITH A 
# MULTIPLE OF SIX MPI TASKS.  Could not get it to
# work otherwise.
#
# Invoke as: sbatch $script
#-----------------------------------------------------------

####SBATCH --ntasks=12 --nodes=2
#SBATCH --ntasks=6 --nodes=1
#SBATCH -t 0:15:00
#SBATCH -A fv3-cpu
#SBATCH -q debug
#SBATCH -J fv3
#SBATCH -o ./log
#SBATCH -e ./log

set -x

source /apps/lmod/lmod/init/ksh
module purge
module load intel/18.1.163
module load impi/5.1.1.109 
module load netcdf/4.3.0
module load slurm/default

# Threads useful when ingesting spectral gfs sigio files.
# Otherwise set to 1.
export OMP_NUM_THREADS=1
export OMP_STACKSIZE=1024M

WORKDIR=/scratch3/NCEPDEV/stmp1/$LOGNAME/chgres_fv3
rm -fr $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

ln -fs ${SLURM_SUBMIT_DIR}/test.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C48.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C48.gaussian.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C48.gfs.gaussian.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C48.gfs.spectral.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C384.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C768.nest.atm.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C768.nest.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C768.atm.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C768.l91.atm.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C1152.l91.atm.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C96.nest.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C768.stretch.theia.nml ./fort.41
#ln -fs ${SLURM_SUBMIT_DIR}/config.C1152.theia.nml ./fort.41

date

srun ${SLURM_SUBMIT_DIR}/../exec/global_chgres.exe

date

exit 0
