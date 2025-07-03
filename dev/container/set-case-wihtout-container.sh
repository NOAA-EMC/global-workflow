#!/bin/bash

 set -x

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
source "${HOMEgfs}/ush/detect_machine.sh"

#yamllist="C48_ATM"
yamllist="C48_S2SW"
#yamllist="C48_S2SWA_gefs"

HOMEDIR=${HOMEgfs}
if [[ ${MACHINE_ID} = ursa* ]] ; then
   rundir=/scratch4/NAGAPE/epic/${USER}/run
   HPC_ACCOUNT=epic
elif [[ ${MACHINE_ID} = gaea* ]] ; then
   rundir=/gpfs/f6/scratch/${USER}/run
   HPC_ACCOUNT=bil-fire8
elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
   TOPICDIR=/bucket/global-workflow-shared-data/ICSDIR
   rundir=/lustre/${USER}/run
   HPC_ACCOUNT=${USER}
fi

 module load rocoto/1.3.7

 rocotocmd=`which rocotorun`

 mkdir -p ${rundir}

 cd ${HOMEDIR}/dev/workflow

 TOPICDIR=${TOPICDIR} \
 RUNTESTS=${rundir} \
 RUNDIRS=${rundir} \
	./generate_workflows.sh \
	-H ${HOMEDIR} \
	-y ${yamllist} \
	-Y ${HOMEDIR}/dev/ci/cases/pr \
	-A ${HPC_ACCOUNT} \
	-e "Wei.Huang@noaa.gov" \
    	-v

