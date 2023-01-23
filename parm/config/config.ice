#! /usr/bin/env bash

case "${ICERES}" in
  "025")
    export NX_GLB="1440"
    export NY_GLB="1080"
    ;;
  "500")  # TODO: From GV, check w/ DW
    export NX_GLB="36"
    export NY_GLB="70"
    ;;
  *)
    echo "FATAL ERROR: Unsupported ICERES = ${ICERES}, ABORT!"
    exit 1
    ;;
esac
