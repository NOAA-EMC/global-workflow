#!/bin/bash

 set -x

#for Ursa
#HOMEDIR=/scratch4/NAGAPE/epic/Wei.Huang/dev/global-workflow-cloud
#container=/scratch4/NAGAPE/epic/Wei.Huang/demo/ubuntu22.04-intel-ufs-env-v1.6.0.img
#rundir=/scratch4/NAGAPE/epic/Wei.Huang/run
#bindings="-B /scratch3 -B /scratch4"
#HPC_ACCOUNT=epic

#for GaeaC6
 HOMEDIR=/gpfs/f6/scratch/Wei.Huang/src/global-workflow-cloud
 container=/gpfs/f6/scratch/Wei.Huang/container/ubuntu22.04-intel-ufs-env-v1.6.0.img
 rundir=/gpfs/f6/scratch/Wei.Huang/run
 bindings="-B /gpfs/f6/scratch -B /ncrc/home1/Wei.Huang"
 HPC_ACCOUNT=bil-fire8

 module load rocoto/1.3.7

 mkdir -p ${rundir}

 ${HOMEDIR}/dev/container/gen-wrapper.sh -H ${HOMEDIR} -c ${container} -b "${bindings}" -v

 cd ${HOMEDIR}/dev/workflow

 RUNTESTS=${rundir} \
	./generate_workflows.sh \
	-H ${HOMEDIR} \
	-y "C96mx100_S2S" \
	-Y ${HOMEDIR}/dev/ci/cases/pr \
	-A ${HPC_ACCOUNT} \
	-e "Wei.Huang@noaa.gov" \
	-r "/autofs/ncrc-svm1_proj/hurr1/hafs/shared/rocoto/1.3.7/bin/rocotorun" \
    	-R -v

 ${HOMEDIR}/dev/container/create-container-links.sh -H ${HOMEDIR}

