#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafs"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################

export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT="$RUNDIR/$CDATE/$CDUMP/wafs.$jobid"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT

export DATA="${DATAROOT}/$job"

###############################################################
echo
echo "=============== START TO RUN WAFS ==============="

# Loop through fcsthrs
hr=0
while [ $hr -le 120 ]; do

  if [ $hr -le 100 ]; then
    export fcsthrs="$(printf "%02d" $(( 10#$hr )) )"
  else
    export fcsthrs=$hr
  fi

  # Execute the JJOB
  $HOMEgfs/jobs/JGFS_ATMOS_WAFS
  status=$?
  [[ $status -ne 0 ]] && exit $status

  hr=$(expr $hr + 6)

done

###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi


exit 0
