#!/bin/bash
#SBATCH --job-name=compile
#SBATCH --account=$USER
#SBATCH --qos=batch
##SBATCH --partition=compute
#SBATCH --partition=process
#SBATCH -t 04:15:00
#SBATCH --nodes=1
#SBATCH -o compile.%J.log
#SBATCH --exclusive

set -x

gwhome=/contrib/Wei.Huang/src/global-workflow-cloud
img=/contrib/Wei.Huang/container/ubuntu22.04-intel-ufs-env-v1.6.0.img
cmd=${gwhome}/sorc/com.sh

singularity exec -B /contrib -B /lustre ${img} ${cmd}

