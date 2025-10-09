#!/bin/bash

set -x

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
source "${HOMEgfs}/ush/detect_machine.sh"

run_with_container="YES"

 casetype="pr"
#yamllist="C48_ATM"
 yamllist="C48_S2SW"
#yamllist="C48_S2SWA_gefs"
#yamllist="C96mx100_S2S"

#casetype=hires
#yamllist="C768_S2SW"

HOMEDIR=${HOMEgfs}
img=ubuntu22.04-intel-ufs-env-v1.9.2.img
if [[ ${MACHINE_ID} = ursa* ]] ; then
   container=/scratch3/NCEPDEV/nems/role.epic/containers/${img}
   rundir=/scratch3/NAGAPE/epic/${USER}/run
   bindings="-B /scratch3 -B /scratch4"
   HPC_ACCOUNT=epic

   module load rocoto/1.3.7
   rocotocmd=$(command -v rocotorun)

   if [[ "${run_with_container}" == "YES" ]]; then
      cp "${HOMEgfs}/env/URSA.env.container" "${HOMEgfs}/env/URSA.env"
   fi
elif [[ ${MACHINE_ID} = gaea* ]] ; then
   container=/gpfs/f6/scratch/Wei.Huang/container/${img}
   rundir=/gpfs/f6/scratch/${USER}/run
   bindings="-B /gpfs/f6/scratch -B /ncrc/home1/${USER}"
   HPC_ACCOUNT=bil-fire8

   rocotocmd=/autofs/ncrc-svm1_home2/Christopher.W.Harrop/rocoto-1.3.7/bin/rocotorun
   if [[ "${run_with_container}" == "YES" ]]; then
      cp "${HOMEgfs}/env/GAEAC6.env.container" "${HOMEgfs}/env/GAEAC6.env"
   fi
elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
   TOPICDIR=/bucket/global-workflow-shared-data/ICSDIR
   container=/contrib/containers/${img}
   rundir=/lustre/${USER}/run
   bindings="--env \"I_MPI_FABRICS=shm:ofi,I_MPI_DEBUG=6\" -B /apps/slurm/default/lib/libpmi2.so -B /contrib -B /lustre -B /bucket"
  #bindings="-B /apps/slurm/default/lib/libpmi2.so -B /contrib -B /lustre -B /bucket"
   HPC_ACCOUNT=${USER}

   module load rocoto/1.3.7
   rocotocmd=$(command -v rocotorun)
fi

set -x

mkdir -p "${rundir}"
mkdir -p "${HOMEDIR}"/ush/container

cd "${HOMEDIR}/dev/workflow" || exit 1

if [[ "${run_with_container}" == "YES" ]]; then
   "${HOMEDIR}/dev/container/utils/gen-wrapper.sh" -H "${HOMEDIR}" -c "${container}" -b "${bindings}" -v

   TOPICDIR=${TOPICDIR} \
   RUNTESTS=${rundir} \
   RUNDIRS=${rundir} \
	./generate_workflows.sh \
	-H "${HOMEDIR}" \
	-y "${yamllist}" \
	-Y "${HOMEDIR}/dev/ci/cases/${casetype}" \
	-A "${HPC_ACCOUNT}" \
	-e "${USER}@noaa.gov" \
	-r "${rocotocmd}" \
    	-v -R

   "${HOMEDIR}/dev/container/utils/create-atmos-products.sh" -H "${HOMEDIR}" -c "${container}" -b "${bindings}"
   "${HOMEDIR}/dev/container/utils/create-container-links.sh" -H "${HOMEDIR}" -c "${container}" -b "${bindings}" -M "${MACHINE_ID}"
else
   TOPICDIR=${TOPICDIR} \
   RUNTESTS=${rundir} \
   RUNDIRS=${rundir} \
        ./generate_workflows.sh \
        -H "${HOMEDIR}" \
        -y "${yamllist}" \
        -Y "${HOMEDIR}/dev/ci/cases/${casetype}" \
        -A "${HPC_ACCOUNT}" \
        -e "${USER}@noaa.gov" \
        -v
fi

