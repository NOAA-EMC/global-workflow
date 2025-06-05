#!/usr/bin/env bash

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1
#export FPATH=/usr/lmod/lmod/libexec
#module reset
#module use ${HOMEgfs}/modulefiles
#module load module_base.container
source /opt/spack-stack/spack-stack-1.6.0/envs/unified-env/install/intel/2021.10.0/intel-oneapi-mpi-2021.9.0-6bnjcwc/setvars.sh --force
export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2023.2.3/linux/compiler/lib/intel64_lin:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2024.0/lib:$LD_LIBRARY_PATH

#export I_MPI_OFI_PROVIDER=tcp
#export I_MPI_FABRICS=shm:ofi
#export FI_PROVIDER=tcp
export HOMEgfs=/scratch4/NAGAPE/epic/Wei.Huang/demo/global-workflow-cloud
arg="$@"
${HOMEgfs}/sorc/ufs_model.fd/tests/gfs_model.x $arg

