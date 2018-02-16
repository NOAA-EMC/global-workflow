#!/bin/ksh -x

. $EXPDIR/config.base
status=$?
[[ $status -ne 0 ]] && exit $status

$HOMEgsi/jobs/JGDAS_ENKF_UPDATE
status=$?
exit $status
