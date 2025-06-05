#!/bin/bash
 containerdir=/scratch4/NAGAPE/epic/Wei.Huang/demo
 img=${containerdir}/ubuntu22.04-intel-ufs-env-v1.6.0.img
 HOMEgfs=/scratch4/NAGAPE/epic/Wei.Huang/demo/global-workflow-cloud
 cmd=${HOMEgfs}/ush/run_wgrib2.sh

 source /usr/lmod/lmod/init/bash
 module purge
 source ${HOMEgfs}/versions/run.ver
 module use ${HOMEgfs}/modulefiles
 module load module_base.container

 arg="$@"
 singularity exec \
        -B /scratch3 \
        -B /scratch4 \
        ${img} $cmd $arg

