#!/bin/ksh -x

. $EXPDIR/config.base
status=$?
[[ $status -ne 0 ]] && exit $status

$HOMEgfs/jobs/JGLOBAL_FORECAST
status=$?
exit $status
