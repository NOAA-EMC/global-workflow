export GFS_CI_ROOT=/work2/noaa/stmp/cmartin/CI/GFSApp
export GFS_CI_HOST='orion'
export GFS_MODULE_USE=$GFS_CI_ROOT/repo/modulefiles
export SLURM_ACCOUNT=da-cpu
export SALLOC_ACCOUNT=$SLURM_ACCOUNT
export SBATCH_ACCOUNT=$SLURM_ACCOUNT
export SLURM_QOS=debug
export SLURM_EXCLUSIVE=user
export OMP_NUM_THREADS=1
ulimit -s unlimited
