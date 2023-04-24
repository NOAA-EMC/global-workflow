#!/usr/bin/bash
export GFS_CI_ROOT=/scratch1/NCEPDEV/global/Terry.McGuinness/GFS_CI_ROOT
export SLURM_ACCOUNT=fv3-cpu
export SALLOC_ACCOUNT="${SLURM_ACCOUNT}"
export SBATCH_ACCOUNT="${SLURM_ACCOUNT}"
export SLURM_QOS=debug
export ICSDIR_ROOT="/scratch1/NCEPDEV/global/glopara/data/ICSDIR"
