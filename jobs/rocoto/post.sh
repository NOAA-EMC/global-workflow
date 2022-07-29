#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
## NCEP post driver script
## FHRGRP : forecast hour group to post-process (e.g. 0, 1, 2 ...)
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

export COMPONENT=${COMPONENT:-atmos}

if [ $FHRGRP = 'anl' ]; then
    fhrlst="anl"
    restart_file=$ROTDIR/${CDUMP}.${PDY}/${cyc}/$COMPONENT/${CDUMP}.t${cyc}z.atm
else
    fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')
    restart_file=$ROTDIR/${CDUMP}.${PDY}/${cyc}/$COMPONENT/${CDUMP}.t${cyc}z.logf
fi


#---------------------------------------------------------------
for fhr in $fhrlst; do
    export post_times=$fhr
    $HOMEgfs/jobs/JGLOBAL_ATMOS_NCEPPOST
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

exit 0
