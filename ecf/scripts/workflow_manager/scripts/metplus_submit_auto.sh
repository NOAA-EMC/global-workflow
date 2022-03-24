#!/bin/sh

set -x

export PSLOT=$PSLOT
export EXPDIR=$EXPDIR
export SDATE=$PDYBEG
export EDATE=$PDYEND

export HOMEgfs=${HOMEgfs:-/lfs/h2/emc/global/noscrub/$USER/para/packages/gfs.v16.2.0}
export HOMEverif_global=$HOMEgfs/sorc/verif-global.fd
runvrfysh=$HOMEgfs/ecf/scripts/workflow_manager/scripts/run_verif_global.sh

#### Modify here if you only need to rerun a list of jobs
vlist="GRID2GRID GRID2OBS PRECIP FIT2OBS MAPS2D MAPSDA"
#### vlist="PRECIP"
#### vlist="MAPS2D"

for vtype in $vlist; do
    export gfsv16_start_date=$PDYBEG
    export gfsv16_end_date=$PDYEND

#   Note: reset MAPSDA gfsv16_start_date below (same as vsdb)

    #### export OUTPUTROOT=/gpfs/dell2/stmp/$LOGNAME/verif_global_standalone_${vtype}.$$
    export OUTPUTROOT=/lfs/h2/emc/stmp/${USER}/RUNDIRS/${PSLOT}/verif_global_standalone_${vtype}.$$

    RUN_GRID2GRID_STEP2=NO
    RUN_GRID2OBS_STEP2=NO
    RUN_PRECIP_STEP2=NO
    RUN_FIT2OBS_PLOTS=NO
    RUN_MAPS2D=NO
    RUN_MAPSDA=NO

    if [ "$vtype" = "GRID2GRID" ]; then
	export RUN_GRID2GRID_STEP2=YES
    elif [ "$vtype" = "GRID2OBS" ]; then
	export RUN_GRID2OBS_STEP2=YES
    elif [ "$vtype" = "PRECIP" ]; then
	export RUN_PRECIP_STEP2=YES
    elif [ "$vtype" = "FIT2OBS" ]; then
        export RUN_FIT2OBS_PLOTS=YES
    elif [ "$vtype" = "MAPS2D" ]; then
	export RUN_MAPS2D=YES
    elif [ "$vtype" = "MAPSDA" ]; then
	export RUN_MAPSDA=YES
#       Limit start date to be within TIME_WINDOW_HOURS of end date.
#       enkf mean and spread plots get input files from prod and para
#       enkfgdas directories.  If TIME_WINDOW_HOURS is too large
#       (generally > 4 days), enkfgdas files for the start date will
#       not be on disk due to scrubber.
        export TIME_WINDOW_HOURS=${TIME_WINDOW_HOURS:-96}
        export gfsv16_start_date=$($NDATE -${TIME_WINDOW_HOURS} ${gfsv16_end_date}00)
        if [[ "$gfsv16_start_date" -lt "${PDYBEG}00" ]]; then
            export gfsv16_start_date=${PDYBEG}00
        fi
        export gfsv16_start_date=$(echo $gfsv16_start_date | cut -c1-8)
    fi

####    $runvrfysh $EXPDIR/scripts/config.vrfy
    $runvrfysh $HOMEgfs/ecf/scripts/workflow_manager/scripts/config.vrfy

done
