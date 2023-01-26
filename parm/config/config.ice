#! /usr/bin/env bash

case "${ICERES}" in
  "025")
    export NX_GLB="1440"
    export NY_GLB="1080"
    ;;
  "500")
    export NX_GLB="72"
    export NY_GLB="35"
    export cice_processor_shape="slenderX1"
    ;;
  *)
    echo "FATAL ERROR: Unsupported ICERES = ${ICERES}, ABORT!"
    exit 1
    ;;
esac
