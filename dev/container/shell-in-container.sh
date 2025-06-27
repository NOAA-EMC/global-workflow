#!/bin/bash

#export SINGULARITY_BIND="${slurm_binding}/lustre:/lustre,/bucket:/bucket,/contrib:/contrib"
#export SINGULARITY_BIND="/lustre:/lustre,/bucket:/bucket,/contrib:/contrib"

img=/contrib/Wei.Huang/src/gw-container-spack-stack-1.6.0/ubuntu22.04-intel-ufs-env-v1.6.0.img
singularity shell -B /contrib -B /lustre ${img}

