#!/bin/bash
#SBATCH --job-name=compile
#SBATCH --account=epic
#SBATCH --qos=batch
#SBATCH --partition=u1-compute
#SBATCH -t 04:15:00
#SBATCH --nodes=1
#SBATCH --ntasks=24
#SBATCH -o compile.%J.log

gwhome=/scratch4/NAGAPE/epic/Wei.Huang/src/container/global-workflow-cloud
cmd=${gwhome}/dev/container/utils/compile-gw-in-container.sh
img=/scratch3/NCEPDEV/nems/role.epic/containers/ubuntu22.04-intel-ufs-env-v1.9.2.img

gw_sorc_dir=${gwhome}/sorc

singularity exec -B /scratch3 -B /scratch4 "${img}" "${cmd}" "${gw_sorc_dir}"

