#!/bin/bash

verbose=false

while [[ "$#" -gt 0 ]]; do
  case "$1" in
    -H|--HOMEgfs)
      HOMEgfs="$2"
      shift 2
      ;;
    -b|--bindings)
      bindings="$2"
      shift 2
      ;;
    -c|--container)
      container="$2"
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

if [[ ! -v HOMEgfs || ! -v container ]]; then
   echo "Usage: link_model.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image -b/--bindings -B dirname [-B dirname1 [...]] [-v]"
   exit 11
fi

if [[ "${verbose}" == "true" ]]; then
   set -x
fi

for dnm in exec ush
do
    if [[ "${dnm}" == "exec" ]]; then
         targetdir=${HOMEgfs}/${dnm}
    else
         targetdir=${HOMEgfs}/${dnm}/container
    fi
    mkdir -p "${targetdir}"
    sourcef=${HOMEgfs}/dev/container/utils/${dnm}.python
    targetf=${targetdir}/run_python.sh

    sed -e "s?HOMEgfs?${HOMEgfs}?g" \
        -e "s?SIF?${container}?g" \
        -e "s?BINDINGS?${bindings}?g" \
	   "${sourcef}" > "${targetf}"

    chmod 755 "${targetf}"
done

sed -i 's/RUN_WITH_CONTAINER=NO/RUN_WITH_CONTAINER=YES/g' "${HOMEgfs}/ush/preamble.sh"

