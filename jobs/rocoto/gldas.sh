#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Execute the JJOB. GLDAS only runs once per day.

if [ $cyc -eq $gldas_cyc ]; then
    $HOMEgfs/jobs/JGDAS_GLDAS
    status=$?
else
    echo "GLDAS only runs for $gldas_cyc cycle; Skip GLDAS step for cycle $cyc"
    status=0
fi

exit $status
