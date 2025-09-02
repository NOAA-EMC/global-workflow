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

echo "HOMEgfs: $HOMEgfs"
echo "container: $container"
echo "bindings: $bindings"
echo "Verbose: $verbose"

sed -e "s?HOMEgfs?${HOMEgfs}?g" \
    -e "s?SIF?${container}?g" \
    -e "s?BINDINGS?${bindings}?g" \
   ${HOMEgfs}/dev/container/utils/exec.exglobal_atmos_products.sh > ${HOMEgfs}/exec/exglobal_atmos_products.sh
   chmod +x ${HOMEgfs}/exec/exglobal_atmos_products.sh

