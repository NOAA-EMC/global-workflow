#!/usr/bin/env bash

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1
export FPATH=/usr/lmod/lmod/libexec
export HOMEgfs=/opt/global-workflow-cloud
source ${HOMEgfs}/versions/run.ver
source /usr/lmod/lmod/init/bash
#module reset
module use ${HOMEgfs}/modulefiles
module load module_base.container
export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2023.2.3/linux/compiler/lib/intel64_lin:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2024.0/lib:$LD_LIBRARY_PATH
${HOMEgfs}/sorc/ufs_model.fd/tests/gfs_model.x

