#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## Abstract:
## Inline METplus verification and diagnostics driver script
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
## SDATE_GFS  : first date of GFS cycle (YYYYMMDDHHMM)
## METPCASE : METplus verification use case (g2g1 | g2o1 | pcp1)
###############################################################

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="metp${METPCASE}"
export jobid="${job}.$$"

##############################################
# make temp directory
##############################################
export DATA=${DATA:-${DATAROOT}/${jobid}}
mkdir -p ${DATA}
cd ${DATA}


##############################################
# Run setpdy and initialize PDY variables
##############################################
export cycle="t${cyc}z"
setpdy.sh
. ./PDY

###############################################################
echo
echo "=============== START TO SOURCE RELEVANT CONFIGS ==============="
configs="base metp"
for config in ${configs}; do
    . ${EXPDIR}/config.${config}
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done


###############################################################
echo
echo "=============== START TO SOURCE MACHINE RUNTIME ENVIRONMENT ==============="
. ${BASE_ENV}/${machine}.env metp
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################
export COMPONENT="atmos"
export VDATE="$(echo $(${NDATE} -${VRFYBACK_HRS} ${CDATE}) | cut -c1-8)"
export COMIN="${ROTDIR}/${CDUMP}.${PDY}/${cyc}/${COMPONENT}"

# TODO: This should not be permitted as DATAROOT is set at the job-card level.
# TODO: DATAROOT is being used as DATA in metp jobs.  This should be rectified in metp.
# TODO: The temporary directory is DATA and is created at the top of the J-Job.
# TODO: remove this line
export DATAROOT=${DATA}

###############################################################
echo
echo "=============== START TO RUN METPLUS VERIFICATION ==============="
if [ ${CDUMP} = "gfs" ]; then

    if [ ${RUN_GRID2GRID_STEP1} = "YES" -o ${RUN_GRID2OBS_STEP1} = "YES" -o ${RUN_PRECIP_STEP1} = "YES" ]; then

        ${VERIF_GLOBALSH}
        status=$?
        [[ ${status} -ne 0 ]] && exit ${status}
        [[ ${status} -eq 0 ]] && echo "Succesfully ran ${VERIF_GLOBALSH}"
    fi
fi


if [ ${CDUMP} = "gdas" ]; then
    echo "METplus verification currently not supported for CDUMP=${CDUMP}"
fi
###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf ${DATAROOT} ; fi  # TODO: This should be $DATA


exit 0
