#!/bin/bash -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status
if [[ $CDUMP == "gefs" ]]; then
    export MEMBER=`echo ${RUNMEM:-"c00"}|cut -c2-3`
    if [[ $RUNMEM != "c00" ]]; then
        export PREFIX_ATMINC="r"
    fi
fi
###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_FORECAST
status=$?
exit $status
