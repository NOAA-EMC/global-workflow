#!/bin/bash
#SBATCH --job-name=test-lustre
#SBATCH --account=$USER
#SBATCH --qos=batch
#SBATCH --partition=compute
#SBATCH -t 04:15:00
#SBATCH --nodes=1
##SBATCH --tasks-per-node=36
##SBATCH --cpus-per-task=1
#SBATCH -o gfsfcst.%J.log
##SBATCH --export=NONE
#SBATCH --exclusive

set -x

cd /contrib/Wei.Huang/src/global-workflow-cloud/sorc

build_all.sh -guw

