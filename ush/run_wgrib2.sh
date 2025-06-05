#!/usr/bin/env bash

export HOMEgfs=/scratch4/NAGAPE/epic/Wei.Huang/demo/global-workflow-cloud

source /usr/lmod/lmod/init/bash
module purge
module use ${HOMEgfs}/modulefiles
module load module_gwsetup.container

module load wgrib2/2.0.8
export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2024.0/lib:$LD_LIBRARY_PATH

arg="$@"

/opt/spack-stack/spack-stack-1.6.0/envs/unified-env/install/intel/2021.10.0/wgrib2-2.0.8-bq36dgw/bin/wgrib2 $arg

