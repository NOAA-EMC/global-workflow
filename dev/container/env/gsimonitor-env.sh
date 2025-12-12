#!/bin/bash

source /usr/lmod/lmod/init/bash
module use "${HOMEgfs}/sorc/gsi_monitor.fd/modulefiles"
module load container.intel

if [[ $# -gt 0 ]]; then
    "$@"
fi

