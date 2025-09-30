#!/bin/bash

set -x

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
source "${HOMEgfs}/ush/detect_machine.sh"
sif=ubuntu22.04-intel-ufs-env-v1.9.2.img

if [[ ${MACHINE_ID} = ursa* ]] ; then
   img=/scratch3/NCEPDEV/nems/role.epic/containers/${sif}
   bindings="-B /scratch3 -B /scratch4"
elif [[ ${MACHINE_ID} = gaea* ]] ; then
   img=/gpfs/f6/scratch/${USER}/container/${sif}
   bindings="-B /gpfs/f6/scratch -B /ncrc/home1/${USER}"
elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
   img=/contrib/containers/${sif}
   bindings="-B /contrib -B /lustre -B /bucket"
fi

singularity shell -e ${bindings} "${img}"

