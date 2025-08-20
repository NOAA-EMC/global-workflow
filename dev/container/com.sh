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

gwhome=/contrib/Wei.Huang/container/global-workflow-cloud
img=/contrib/Wei.Huang/container/ubuntu22.04-intel-ufs-env-v1.9.2.img
cmd=${gwhome}/dev/container/utils/compile-gw-in-container.sh

gw_sorc_dir=${gwhome}/sorc

singularity exec -B /contrib ${img} ${cmd} ${gw_sorc_dir}

