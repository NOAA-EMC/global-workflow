#!/usr/bin/bash

export GFS_CI_ROOT=/work2/noaa/global/mterry/GFS_CI_ROOT
export ICSDIR_ROOT=/work/noaa/global/glopara/data/ICSDIR
export SLURM_ACCOUNT=nems
export SALLOC_ACCOUNT=${SLURM_ACCOUNT}
export SBATCH_ACCOUNT=${SLURM_ACCOUNT}
export SLURM_QOS=debug
export SLURM_EXCLUSIVE=user
export OMP_NUM_THREADS=1
ulimit -s unlimited
