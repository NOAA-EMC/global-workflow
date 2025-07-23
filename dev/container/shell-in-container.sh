#!/bin/bash

set -x

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
source "${HOMEgfs}/ush/detect_machine.sh"
sif=ubuntu22.04-intel-ufs-env-v1.6.0.img

if [[ ${MACHINE_ID} = ursa* ]] ; then
   img=/scratch4/NAGAPE/epic/${USER}/demo/${sif}
   bindings="-e -B /scratch3 -B /scratch4"
elif [[ ${MACHINE_ID} = gaea* ]] ; then
   img=/gpfs/f6/scratch/${USER}/container/${sif}
   bindings="-e -B /gpfs/f6/scratch -B /ncrc/home1/${USER}"
elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
   img=/contrib/${USER}/src/gw-container-spack-stack-1.6.0/${sif}
   bindings="-e -B /contrib -B /lustre -B /bucket"
fi

cd ${HOMEDIR}/dev/workflow

singularity shell ${bindings}  ${img}

