#!/bin/bash
#img=/scratch4/NAGAPE/epic/Wei.Huang/src/container/gw-container
 containerdir=/contrib/Wei.Huang/src/gw-container-spack-stack-1.6.0
 img=${containerdir}/wei-gw-container
 cmd=/opt/global-workflow-cloud/ush/run_wgrib2.sh
 arg="$@"
 echo "running: singularity exec ${img} $cmd $arg"
 singularity exec \
        -B /contrib \
        -B /lustre \
        ${img} $cmd $arg

