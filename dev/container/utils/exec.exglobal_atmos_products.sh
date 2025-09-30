#!/bin/bash
 LD_LIBRARY_PATH=$(dirname SIF)
 export LD_LIBRARY_PATH

 singularity exec \
        BINDINGS \
        SIF \
        "HOMEgfs/scripts/exglobal_atmos_products.sh" "$@"

