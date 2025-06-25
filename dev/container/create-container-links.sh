#!/bin/bash

verbose=true

#echo "Verbose: $verbose"

source ${HOMEgfs}/ush/detect_machine.sh

bindings="-B /scratch3 -B /scratch4"
if [[ ${MACHINE_ID} = ursa* ]] ; then
    echo "We are on NOAA Ursa"
    bindings="-B /scratch3 -B /scratch4"
    HOMEgfs=/scratch4/NAGAPE/epic/Wei.Huang/dev/global-workflow-cloud
    container=/scratch4/NAGAPE/epic/Wei.Huang/demo/ubuntu22.04-intel-ufs-env-v1.6.0.img
elif [[ ${MACHINE_ID} = gaea* ]] ; then
    echo "We are on NOAA Gaea"
    bindings="-B /gpfs/f6/scratch"
    HOMEgfs=/gpfs/f6/scratch/Wei.Huang/src/global-workflow-cloud
    container=/gpfs/f6/scratch/Wei.Huang/container/ubuntu22.04-intel-ufs-env-v1.6.0.img
fi

#echo "HOMEgfs: $HOMEgfs"
#echo "container: $container"

#${HOMEgfs}/dev/container/gen-wrapper.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"

${HOMEgfs}/dev/container/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t gfs
${HOMEgfs}/dev/container/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t sfs
${HOMEgfs}/dev/container/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t gefs

${HOMEgfs}/dev/container/link_model.sh -H ${HOMEgfs} -c ${container} -m gfs_model -b "${bindings}"
${HOMEgfs}/dev/container/link_model.sh -H ${HOMEgfs} -c ${container} -m sfs_model -b "${bindings}"
${HOMEgfs}/dev/container/link_model.sh -H ${HOMEgfs} -c ${container} -m gefs_model -b "${bindings}"

${HOMEgfs}/dev/container/link_gfs_utils.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"
${HOMEgfs}/dev/container/link_ufs_utils.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"

