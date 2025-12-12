#!/bin/bash

source /usr/lmod/lmod/init/bash
module use "${HOMEgfs}/sorc/gsi_utils.fd/modulefiles"
module load gsiutils_container.intel

if [[ $# -gt 0 ]]; then
    "$@"
fi

