#!/bin/bash

HOMEgfs=/gpfs/f6/scratch/Wei.Huang/src/global-workflow-cloud
verbose=false

while [ "$#" -gt 0 ]; do
  case "$1" in
    -H|--HOMEgfs)
      HOMEgfs="$2"
      shift 2
      ;;
    -v|--verbose)
      verbose=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

if [[ ! -v HOMEgfs ]]; then
   echo "Usage: create-container-links.sh -H/--HOMEgfs gw-home-dir [-v]"
   exit -1
fi

source ${HOMEgfs}/ush/detect_machine.sh

echo "MACHINE_ID: ${MACHINE_ID}"

bindings="-B /scratch3 -B /scratch4"
if [[ ${MACHINE_ID} = ursa* ]] ; then
    echo "We are on NOAA Ursa"
    bindings="-B /scratch3 -B /scratch4"
    container=/scratch4/NAGAPE/epic/Wei.Huang/demo/ubuntu22.04-intel-ufs-env-v1.6.0.img
elif [[ ${MACHINE_ID} = gaea* ]] ; then
    echo "We are on NOAA Gaea"
    bindings="-B /gpfs/f6/scratch"
    container=/gpfs/f6/scratch/Wei.Huang/container/ubuntu22.04-intel-ufs-env-v1.6.0.img
fi

echo "HOMEgfs: $HOMEgfs"
echo "container: $container"
echo "bindings: $bindings"
echo "Verbose: $verbose"

#${HOMEgfs}/dev/container/gen-wrapper.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"

${HOMEgfs}/dev/container/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t gfs
${HOMEgfs}/dev/container/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t sfs
${HOMEgfs}/dev/container/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t gefs

${HOMEgfs}/dev/container/link_model.sh -H ${HOMEgfs} -c ${container} -m gfs_model -b "${bindings}"
${HOMEgfs}/dev/container/link_model.sh -H ${HOMEgfs} -c ${container} -m sfs_model -b "${bindings}"
${HOMEgfs}/dev/container/link_model.sh -H ${HOMEgfs} -c ${container} -m gefs_model -b "${bindings}"

${HOMEgfs}/dev/container/link_gfs_utils.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"
${HOMEgfs}/dev/container/link_ufs_utils.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"

