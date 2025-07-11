#!/bin/bash

set -x

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
source "${HOMEgfs}/ush/detect_machine.sh"

if [[ ${MACHINE_ID} = ursa* ]] ; then
   container=/scratch4/NAGAPE/epic/${USER}/demo/ubuntu22.04-intel-ufs-env-v1.6.0.img
   bindings="-B /scratch3 -B /scratch4 -B /home/${USER}"
elif [[ ${MACHINE_ID} = gaea* ]] ; then
   container=/gpfs/f6/scratch/${USER}/container/ubuntu22.04-intel-ufs-env-v1.6.0.img
   bindings="-B /gpfs/f6/scratch -B /ncrc/home1/${USER}"
elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
   container=/contrib/${USER}/src/gw-container-spack-stack-1.6.0/ubuntu22.04-intel-ufs-env-v1.6.0.img
   bindings="-B /contrib -B /lustre"
fi

singularity shell ${bindings} ${container}

