#!/bin/bash

 set -x

 HOMEDIR=/scratch4/NAGAPE/epic/Wei.Huang/dev/global-workflow-cloud
 rundir=/scratch4/NAGAPE/epic/Wei.Huang/run
 mkdir -p ${rundir}
 HPC_ACCOUNT=epic

 cd ${HOMEDIR}/dev/workflow

 RUNTESTS=${rundir} \
	./generate_workflows.sh \
	-H ${HOMEDIR} \
	-y "C48_ATM" \
	-Y ${HOMEDIR}/dev/ci/cases/pr \
	-A ${HPC_ACCOUNT} \
	-e "Wei.Huang@noaa.gov" \
    	-R -v

