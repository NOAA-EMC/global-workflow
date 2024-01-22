#!/bin/bash

set -eu

HOMEgfs=$1
pslot=$2

echo -e "HOMEgfs: ${HOMEgfs}\npslpot: ${pslot}\n in ${PWD}" >> "${HOMEgfs}/ci/scripts/run-check_ci_stub.log"
if [[ ${pslot} =~ "C48_S2SW" ]]; then
     exit 1
fi
