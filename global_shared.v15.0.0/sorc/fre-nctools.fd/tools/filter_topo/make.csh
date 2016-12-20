#!/bin/csh -f

source /opt/modules/default/init/csh
#source $PWD/../../../../IC_scripts/ENV.GAEA
source ${PWD}/../../../../modulefiles/fv3gfs/fre-nctools.cray

set echo 
ftn -o filter_topo -fltconsistency -fno-alias -stack_temps -safe_cray_ptr -ftz -assume byterecl -g -O2 -i4 -real_size 64 -traceback filter_topo.F90



