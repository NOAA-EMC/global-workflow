#!/bin/ksh

###############################################################
## NCEP post driver script
## FHRGRP : forecast hour group to post-process (e.g. 0, 1, 2 ...)
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
set -x

. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

configs="base post"
config_path=${EXPDIR:-$NWROOT/gfs.${gfs_ver}/parm/config}
for config in $configs; do
    . $config_path/config.$config
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

if [ $RUN_ENVIR = "nco" ]; then
    export COMIN=${COMIN:-$ROTDIR/$RUN.$PDY/$cyc}
    export COMOUT=${COMOUT:-$ROTDIR/$RUN.$PDY/$cyc}
else
    export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
    export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
fi
[[ ! -d $COMOUT ]] && mkdir -m 775 -p $COMOUT

if [ $FHRGRP -eq 'anl' ]; then
    fhrlst="anl"
    restart_file=$ROTDIR/${CDUMP}.${PDY}/${cyc}/${CDUMP}.t${cyc}z.atm
else
    fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/\[/ /g; s/\]/ /g; s/f/ /g; s/,/ /g')
    restart_file=$ROTDIR/${CDUMP}.${PDY}/${cyc}/${CDUMP}.t${cyc}z.logf
fi


#---------------------------------------------------------------
for fhr in $fhrlst; do

    if [ ! -f $restart_file${fhr}.nemsio ]; then
        echo "Nothing to process for FHR = $fhr, cycle"
        continue
    fi

    export post_times=$fhr
    $HOMEgfs/jobs/JGLOBAL_NCEPPOST
    status=$?
    [[ $status -ne 0 ]] && exit $status

done

###############################################################
# Exit out cleanly
exit 0
