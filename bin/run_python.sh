#!/bin/bash
 containerdir=/scratch4/NAGAPE/epic/Wei.Huang/demo
 img=${containerdir}/ubuntu22.04-intel-ufs-env-v1.6.0.img
 HOMEgfs=/scratch4/NAGAPE/epic/Wei.Huang/demo/global-workflow-cloud
 cmd=${HOMEgfs}/ush/run_python.sh
 arg="$@"

#wxflowPATH="${HOMEgfs}/ush/python"
#export PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush:${wxflowPATH}"

 singularity exec \
        -B /scratch3 \
        -B /scratch4 \
        ${img} $cmd $arg

