#!/bin/bash

set -eu

HOMEgfs=$1
pslot=$2

echo -e "HOMEgfs: ${HOMEgfs}\npslpot: ${pslot}\n in ${PWD}" >> ${HOMEgfs}/ci/scripts/run-check_ci_stub.log