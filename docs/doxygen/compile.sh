#!/bin/bash

set -ex

machine=${1:-${machine:-"URSA"}}
machine=${machine^^}

case "${machine}" in
    "URSA")
        module load doxygen/1.11.0
        doxygen=/apps/spack-2024-12/linux-rocky9-x86_64/gcc-11.4.1/doxygen-1.11.0-foxxc5vusekhwlyzozxdhgggmykepebg/bin/doxygen
        ;;
    "ORION")
        doxygen=/bin/doxygen
        ;;
    *)
        echo "Machine ${machine} is unrecognized!"
        echo "Trying system doxygen"
        doxygen=$(command -v doxygen)
        rc=$?
        if [[ "${rc}" -ne 0 ]]; then
            echo "doxygen not found, ABORT!"
            exit 1
        fi
        ;;
esac

${doxygen}
status=$?
exit "${status}"
