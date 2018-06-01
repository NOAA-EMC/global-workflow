#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

# Execute config.base to define FHMAX_GFS, FHOUT_GFS, and ROTDIR
configs="base"
config_path=${EXPDIR:-$NWROOT/gfs.${gfs_ver}/parm/config}
for config in $configs; do
    . $config_path/config.$config
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_FORECAST
status=$?

# If gfs fcst, create log files out to maximum gfs forecast length
if [ "$CDUMP" = "gfs" -a "$FHMAX_GFS" -lt "384" ]; then
   fhend=384
   fhinc=$FHOUT_GFS
   (( fhbeg = $FHMAX_GFS + $fhinc ))
   for fhr in $(seq $fhbeg $fhinc $fhend); do
       echo "skip $fhr" > $ROTDIR/$CDUMP.$PDY/$cyc/$CDUMP.t${cyc}z.logf$fhr.nemsio
   done
fi

exit $status
