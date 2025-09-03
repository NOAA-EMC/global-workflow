#!/bin/bash

verbose=false

while [ "$#" -gt 0 ]; do
  case "$1" in
    -H|--HOMEgfs)
      HOMEgfs="$2"
      shift 2
      ;;
    -c|--container)
      container="$2"
      shift 2
      ;;
    -b|--bindings)
      bindings="$2"
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

if [[ ! -v HOMEgfs || ! -v container || ! -v bindings ]]; then
   echo "Usage: create-container-links.sh -H/--HOMEgfs gw-home-dir -c/--container container-fullpath -b/--bindings list-of-binding-dirs [-v]"
   exit -1
fi

# echo "HOMEgfs: $HOMEgfs"
# echo "container: $container"
# echo "bindings: $bindings"
# echo "Verbose: $verbose"

${HOMEgfs}/dev/container/utils/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t gfs
${HOMEgfs}/dev/container/utils/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t sfs
${HOMEgfs}/dev/container/utils/link_ww3.sh -H ${HOMEgfs} -c ${container} -b "${bindings}" -t gefs

${HOMEgfs}/dev/container/utils/link_model.sh -H ${HOMEgfs} -c ${container} -m gfs_model -b "${bindings}"
${HOMEgfs}/dev/container/utils/link_model.sh -H ${HOMEgfs} -c ${container} -m sfs_model -b "${bindings}"
${HOMEgfs}/dev/container/utils/link_model.sh -H ${HOMEgfs} -c ${container} -m gefs_model -b "${bindings}"

${HOMEgfs}/dev/container/utils/link_gfs_utils.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"
${HOMEgfs}/dev/container/utils/link_ufs_utils.sh -H ${HOMEgfs} -c ${container} -b "${bindings}"

