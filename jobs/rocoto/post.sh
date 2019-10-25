#!/bin/ksh -x

###############################################################
## NCEP post driver script
## FHRGRP : forecast hour group to post-process (e.g. 0, 1, 2 ...)
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status


if [ $FHRGRP -eq 0 ]; then
    fhrlst="anl"
    restart_file=$ROTDIR/${CDUMP}.${PDY}/${cyc}/${CDUMP}.t${cyc}z.atm
else
    fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')
    restart_file=$ROTDIR/${CDUMP}.${PDY}/${cyc}/${CDUMP}.t${cyc}z.logf
fi


#---------------------------------------------------------------
for fhr in $fhrlst; do

    if [ ! -f $restart_file${fhr}.nemsio -a ! -f $restart_file${fhr}.nc ]; then
        echo "Nothing to process for FHR = $fhr, cycle"
        continue
    fi

    #master=$ROTDIR/${CDUMP}.${PDY}/${cyc}/${CDUMP}.t${cyc}z.master.grb2f${fhr}
    pgb0p25=$ROTDIR/${CDUMP}.${PDY}/${cyc}/${CDUMP}.t${cyc}z.pgrb2.0p25.f${fhr}
    if [ ! -s $pgb0p25 ]; then
        export post_times=$fhr
        $HOMEgfs/jobs/JGLOBAL_NCEPPOST
        status=$?
        [[ $status -ne 0 ]] && exit $status
    fi

done

###############################################################
# Exit out cleanly
exit 0
