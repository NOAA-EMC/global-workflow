#!/bin/bash

verbose=false

while [[ "$#" -gt 0 ]]; do
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
    -M|--MACHINE_ID)
      machineid="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

if [[ ! -v HOMEgfs || ! -v container || ! -v bindings || ! -v MACHINE_ID ]]; then
   echo "Usage: create-container-links.sh -H/--HOMEgfs gw-home-dir -c/--container container-fullpath -b/--bindings -M|--MACHINE_ID list-of-binding-dirs [-v]"
   exit 11
fi

if [[ "${verbose}" == "true" ]]; then
   echo "HOMEgfs: ${HOMEgfs}"
   echo "container: ${container}"
   echo "bindings: ${bindings}"
   echo "Verbose: ${verbose}"
fi

"${HOMEgfs}/dev/container/utils/link_ww3.sh" -H "${HOMEgfs}" -c "${container}" -b "${bindings}" -t gfs
"${HOMEgfs}/dev/container/utils/link_ww3.sh" -H "${HOMEgfs}" -c "${container}" -b "${bindings}" -t sfs
"${HOMEgfs}/dev/container/utils/link_ww3.sh" -H "${HOMEgfs}" -c "${container}" -b "${bindings}" -t gefs

"${HOMEgfs}/dev/container/utils/link_model.sh" -H "${HOMEgfs}" -c "${container}" -m gfs_model -b "${bindings}" -M "${machineid}"
"${HOMEgfs}/dev/container/utils/link_model.sh" -H "${HOMEgfs}" -c "${container}" -m sfs_model -b "${bindings}" -M "${machineid}"
"${HOMEgfs}/dev/container/utils/link_model.sh" -H "${HOMEgfs}" -c "${container}" -m gefs_model -b "${bindings}" -M "${machineid}"

"${HOMEgfs}/dev/container/utils/link_gfs_utils.sh" -H "${HOMEgfs}" -c "${container}" -b "${bindings}"
"${HOMEgfs}/dev/container/utils/link_ufs_utils.sh" -H "${HOMEgfs}" -c "${container}" -b "${bindings}"

"${HOMEgfs}/dev/container/utils/link_gsi.sh" -H "${HOMEgfs}" -c "${container}" -b "${bindings}" -M "${machineid}"

