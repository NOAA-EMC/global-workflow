#!/usr/bin/env bash

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1
export FPATH=/usr/lmod/lmod/libexec
export HOMEgfs=/gpfs/f6/scratch/Wei.Huang/src/global-workflow-cloud
source /usr/lmod/lmod/init/bash
module purge
source ${HOMEgfs}/versions/run.container.ver
module use ${HOMEgfs}/modulefiles
module load module_base.container
export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2023.2.3/linux/compiler/lib/intel64_lin:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2024.0/lib:$LD_LIBRARY_PATH
#export I_MPI_DEBUG=30
#export I_MPI_FABRICS=shm:tcp
#export FI_PROVIDER=shm:tcp
export I_MPI_DEBUG=1
export I_MPI_FABRICS=shm:ofi
export I_MPI_OFI_PROVIDER=tcp
export FI_PROVIDER=tcp
export FI_TCP_IFACE=eth0
args=$@
${HOMEgfs}/sorc/ufs_model.fd/tests/gfs_model.x $args

