#!/bin/bash
set -eu

SECONDS=0

if [[ $# != 4 ]]; then
  echo "Usage: $0 PATHTR MACHINE_ID MAKE_OPT BUILD_NR"
  exit 1
fi

readonly PATHTR=$1
readonly MACHINE_ID=$2
readonly MAKE_OPT=$3
readonly BUILD_NR=$4

hostname

echo "Compiling ${MAKE_OPT}"
cd ${PATHTR}

./configure ${MACHINE_ID}

set +x
if [[ $MACHINE_ID = wcoss ]]; then
  source /usrx/local/Modules/default/init/sh
elif [[ $MACHINE_ID = wcoss_cray ]]; then
  source /opt/modules/default/init/sh
elif [[ $MACHINE_ID = theia ]]; then
  source /apps/lmod/lmod/init/sh
fi
source conf/modules.fv3
module list
set -x

gmake clean

gmake ${MAKE_OPT} -j 8

mv fv3.exe ${PATHTR}/tests/fv3_${BUILD_NR}.exe
cp conf/modules.fv3 ${PATHTR}/tests/modules.fv3_${BUILD_NR}

gmake cleanall

elapsed=$SECONDS
echo "Elapsed time $elapsed seconds. Compiling ${MAKE_OPT} finished"
