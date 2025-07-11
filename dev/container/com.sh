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

container=/scratch4/NAGAPE/epic/${USER}/demo/ubuntu22.04-intel-ufs-env-v1.6.0.img
bindings="-B /scratch3 -B /scratch4 -B /home/${USER}"

singularity exec ${bindings} ${container} \
	/scratch4/NAGAPE/epic/Wei.Huang/src/global-workflow-cloud/dev/container/compile-gw-in-container.sh

