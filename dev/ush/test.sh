#!/usr/bin/env bash
DIR=$1
find "${DIR}" -type f -exec sed -i -e 's/HOMEgfs/HOMEglobal/g' -e 's/PARMgfs/PARMglobal/g' -e 's/USHgfs/USHglobal/g' -e 's/SCRgfs/SCRglobal/g' {} +
