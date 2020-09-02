#!/bin/ksh -x

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

# Load job specific modulefile
module use -a $HOMEgfs/modulefiles
module load modulefile_gfswave_gempak.wcoss_dell_p3
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wavegempak"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
echo
echo "=============== BEGIN TO SOURCE MACHINE RUNTIME ENVIRONMENT ==============="
. $BASE_ENV/${machine}.env wavegempak
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo
echo "=============== START TO RUN WAVE GEMPAK ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_WAVE_GEMPAK
status=$?
exit $status
