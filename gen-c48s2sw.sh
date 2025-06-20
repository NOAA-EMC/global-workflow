#!/bin/bash

 set -x

 HOMEDIR=/scratch4/NAGAPE/epic/Wei.Huang/dev/global-workflow-cloud
 rundir=/scratch4/NAGAPE/epic/Wei.Huang/run
 mkdir -p ${rundir}
 HPC_ACCOUNT=epic

 container=/scratch4/NAGAPE/epic/Wei.Huang/demo/ubuntu22.04-intel-ufs-env-v1.6.0.img
 bindings="-B /scratch3 -B /scratch4"
 ${HOMEDIR}/dev/container/gen-wrapper.sh -H ${HOMEDIR} -c ${container} -b "${bindings}" -v

 cd ${HOMEDIR}/dev/workflow

 RUNTESTS=${rundir} \
	./generate_workflows.sh \
	-H ${HOMEDIR} \
	-y "C48_S2SW" \
	-Y ${HOMEDIR}/dev/ci/cases/pr \
	-A ${HPC_ACCOUNT} \
	-e "Wei.Huang@noaa.gov" \
    	-R -v

 ${HOMEDIR}/dev/container/create-container-links.sh

