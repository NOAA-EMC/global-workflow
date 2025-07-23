#!/bin/bash
 export LD_LIBRARY_PATH=$(dirname SIF)
 arg="$@"

 singularity exec \
        BINDINGS \
        SIF \
        HOMEgfs/ush/container/exglobal_atmos_products.sh $arg

