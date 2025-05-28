#!/bin/bash
#containerdir=/gpfs/f6/scratch/Wei.Huang/container
 containerdir=/contrib/Wei.Huang/src/gw-container-spack-stack-1.6.0
 img=${containerdir}/wei-gw-container
 cmd=/opt/global-workflow-cloud/ush/run_python.sh
 arg="$@"
 echo "running: singularity exec ${img} $cmd $arg"
 singularity exec \
        -B /gpfs/f6/scratch/Wei.Huang \
        ${img} $cmd $arg

