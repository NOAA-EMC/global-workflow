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

# Execute config.base to define ROTDIR
configs="base"
config_path=${EXPDIR:-$NWROOT/gfs.${gfs_ver}/parm/config}
for config in $configs; do
    . $config_path/config.$config
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

if [ $FHRGRP -eq 0 ]; then
    fhrlst="anl"             
else
    fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')
fi

#---------------------------------------------------------------
for fhr in $fhrlst; do

#   Process analysis or fhr for which atmospheric nemsio file exists
    if [ $FHRGRP -eq 0 -o -s $ROTDIR/$CDUMP.$PDY/$cyc/$CDUMP.t${cyc}z.atmf$fhr.nemsio ]; then
	export post_times=$fhr
	$HOMEgfs/jobs/JGLOBAL_NCEPPOST
	status=$?
	[[ $status -ne 0 ]] && exit $status
    fi

done

###############################################################
# Exit out cleanly
exit 0
