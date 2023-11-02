#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="vrfy"
export jobid="${job}.$$"

# TODO (#235) - This job is calling multiple j-jobs and doing too much in general
#   Also, this forces us to call the config files here instead of the j-job
source "${HOMEgfs}/ush/jjob_header.sh" -e "vrfy" -c "base vrfy"

###############################################################
export CDUMP="${RUN/enkf}"

CDATEm1=$(${NDATE} -24 "${PDY}${cyc}")
export CDATEm1
export PDYm1=${CDATEm1:0:8}

###############################################################
# TODO: We can likely drop support for these dev-only grib1 precip files
echo
echo "=============== START TO GENERATE QUARTER DEGREE GRIB1 FILES ==============="
if [[ ${MKPGB4PRCP} = "YES" && ${CDUMP} == "gfs" ]]; then
    YMD=${PDY} HH=${cyc} generate_com -x COM_ATMOS_MASTER
    if [ ! -d ${ARCDIR} ]; then mkdir -p ${ARCDIR} ; fi
    nthreads_env=${OMP_NUM_THREADS:-1} # get threads set in env
    export OMP_NUM_THREADS=1
    cd "${COM_ATMOS_MASTER}" || exit 9
    fhmax=${vhr_rain:-${FHMAX_GFS}}
    for (( fhr=0; fhr <= fhmax; fhr+=6 )); do
       fhr2=$(printf %02i "${fhr}")
       fhr3=$(printf %03i "${fhr}")
       fname=${RUN}.t${cyc}z.sfluxgrbf${fhr3}.grib2
       fileout=${ARCDIR}/pgbq${fhr2}.${RUN}.${PDY}${cyc}.grib2
       ${WGRIB2} "${fname}" -match "(:PRATE:surface:)|(:TMP:2 m above ground:)" -grib "${fileout}"
    done
    export OMP_NUM_THREADS=${nthreads_env} # revert to threads set in env
fi


###############################################################
echo
echo "=============== START TO RUN MOS ==============="
if [[ "${RUNMOS}" == "YES" && "${CDUMP}" == "gfs" ]]; then
    ${RUNGFSMOSSH} "${PDY}${cyc}"
fi


################################################################################
echo
echo "=============== START TO RUN CYCLONE TRACK VERIFICATION ==============="
if [[ ${VRFYTRAK} = "YES" ]]; then

    COMINsyn=${COMINsyn:-$(compath.py "${envir}/com/gfs/${gfs_ver}")/syndat}
    export COMINsyn

    ${TRACKERSH}
fi


################################################################################
echo
echo "=============== START TO RUN CYCLONE GENESIS VERIFICATION ==============="
if [[ ${VRFYGENESIS} = "YES" && "${CDUMP}" = "gfs" ]]; then
    ${GENESISSH}
fi


################################################################################
echo
echo "=============== START TO RUN CYCLONE GENESIS VERIFICATION (FSU) ==============="
if [[ ${VRFYFSU} = "YES" && "${CDUMP}" = "gfs" ]]; then
    ${GENESISFSU}
fi


###############################################################
# Force Exit out cleanly
cd "${DATAROOT}"
if [[ ${KEEPDATA:-"NO"} = "NO" ]] ; then rm -rf "${DATA}" ; fi


exit 0
