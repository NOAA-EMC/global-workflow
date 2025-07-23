#!/bin/bash
 containerdir=/gpfs/f6/scratch/Wei.Huang/container
 img=${containerdir}/ubuntu22.04-intel-ufs-env-v1.6.0.img

 HOMEgfs=/gpfs/f6/scratch/Wei.Huang/src/global-workflow-cloud
 cmd=${HOMEgfs}/ush/run_python.sh
 arg="$@"
 echo "running: singularity exec ${img} $cmd $arg"
 singularity exec \
        -B /gpfs/f6/scratch \
        ${img} $cmd $arg
