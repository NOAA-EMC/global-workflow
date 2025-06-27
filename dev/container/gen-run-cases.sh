#!/bin/bash

 set -x

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
source "${HOMEgfs}/ush/detect_machine.sh"

#yamllist="C48_ATM"
yamllist="C48_S2SW"
#yamllist="C48_S2SWA_gefs"

HOMEDIR=${HOMEgfs}
if [[ ${MACHINE_ID} = ursa* ]] ; then
   container=/scratch4/NAGAPE/epic/${USER}/demo/ubuntu22.04-intel-ufs-env-v1.6.0.img
   rundir=/scratch4/NAGAPE/epic/${USER}/run
   bindings="-B /scratch3 -B /scratch4"
   HPC_ACCOUNT=epic
elif [[ ${MACHINE_ID} = gaea* ]] ; then
   container=/gpfs/f6/scratch/${USER}/container/ubuntu22.04-intel-ufs-env-v1.6.0.img
   rundir=/gpfs/f6/scratch/${USER}/run
   bindings="-B /gpfs/f6/scratch -B /ncrc/home1/${USER}"
   HPC_ACCOUNT=bil-fire8
elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
   TOPICDIR=/bucket/global-workflow-shared-data/ICSDIR
   container=/contrib/${USER}/src/gw-container-spack-stack-1.6.0/ubuntu22.04-intel-ufs-env-v1.6.0.img
   rundir=/lustre/${USER}/run
   bindings="--env \"I_MPI_FABRICS=ofi:shm,I_MPI_DEBUG=6\" -B /apps/slurm/default/lib/libpmi2.so -B /contrib -B /lustre -B /bucket"
   HPC_ACCOUNT=${USER}
fi

 module load rocoto/1.3.7

 rocotocmd=`which rocotorun`

 mkdir -p ${rundir}

 ${HOMEDIR}/dev/container/gen-wrapper.sh -H ${HOMEDIR} -c ${container} -b "${bindings}" -v

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
	-r ${rocotocmd} \
    	-R -v

 ${HOMEDIR}/dev/container/create-container-links.sh -H ${HOMEDIR} -c ${container} -b "${bindings}"

