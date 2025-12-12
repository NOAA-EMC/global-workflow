#!/bin/bash

source /usr/lmod/lmod/init/bash
module use "${HOMEgfs}/sorc/ufs_model.fd/modulefiles"
module load ufs_container.intel

#export LD_LIBRARY_PATH=${CRAY_MPICH_DIR}/lib-abi-mpich:/opt/cray/pe/lib64:$LD_LIBRARY_PATH:/host/usr/lib64

if [[ $# -gt 0 ]]; then
    "$@"
fi

