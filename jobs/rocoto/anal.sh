#!/bin/ksh -x

. $EXPDIR/config.base
status=$?
[[ $status -ne 0 ]] && exit $status

$HOMEgsi/jobs/JGLOBAL_ANALYSIS
status=$?
exit $status
