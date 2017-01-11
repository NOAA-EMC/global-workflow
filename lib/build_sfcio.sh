#!/bin/sh
set -x

. /opt/modules/default/init/sh
. $PWD/../global_shared.v15.0.0/modulefiles/fv3gfs/fre-nctools.cray

set -x

cd sfcio_v1.0.0/sorc
./make.sh

