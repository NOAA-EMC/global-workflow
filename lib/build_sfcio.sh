#!/bin/sh
set -x

. /opt/modules/default/init/sh
. $PWD/../sorc/modulefiles/fre-nctools.cray

set -x

cd sfcio_v1.0.0/sorc
./make.sh

