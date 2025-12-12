#!/bin/bash

source /usr/lmod/lmod/init/bash
module use "${HOMEgfs}/sorc/gfs_utils.fd/modulefiles"
module load gfsutils_container.intel
module load wgrib2
module load gettext
module load prod_util
export UTILROOT=${prod_util_ROOT}

#export LD_LIBRARY_PATH=${CRAY_MPICH_DIR}/lib-abi-mpich:/opt/cray/pe/lib64:$LD_LIBRARY_PATH:/host/usr/lib64

if [[ $# -gt 0 ]]; then
    "$@"
fi

