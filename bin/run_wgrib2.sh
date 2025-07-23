#!/bin/bash
 containerdir=/gpfs/f6/scratch/Wei.Huang/container
 img=${containerdir}/gw-container.sif
 cmd=/opt/global-workflow-cloud/ush/run_wgrib2.sh
 arg="$@"
 echo "running: singularity exec ${img} $cmd $arg"
 singularity exec \
        -B /scratch4 \
        ${img} $cmd $arg
