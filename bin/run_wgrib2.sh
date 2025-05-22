#!/bin/bash
 img=/scratch4/NAGAPE/epic/Wei.Huang/src/container/gw-container
 cmd=/opt/global-workflow-cloud/ush/run_wgrib2.sh
 arg="$@"
 echo "running: singularity exec ${img} $cmd $arg"
 singularity exec \
        -B /scratch4 \
        ${img} $cmd $arg

