#!/bin/ksh -x

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

# Load job specific modulefile
module use -a $HOMEgfs/modulefiles
module load modulefile_gfswave_prdgen.wcoss_dell_p3
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base waveawipsbulls"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
echo
echo "=============== BEGIN TO SOURCE MACHINE RUNTIME ENVIRONMENT ==============="
. $BASE_ENV/${machine}.env waveawipsbulls
status=$?
[[ $status -ne 0 ]] && exit $status

export DBNROOT=/dev/null

###############################################################
echo
echo "=============== START TO RUN WAVE PRDGEN BULLS ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_WAVE_PRDGEN_BULLS
status=$?
exit $status

