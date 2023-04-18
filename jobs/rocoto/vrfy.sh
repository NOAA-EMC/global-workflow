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

CDATEm1c=$(${NDATE} -06 "${PDY}${cyc}")
PDYm1c=${CDATEm1c:0:8}
pcyc=${CDATEm1c:8:2}


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
    fhr=0
    while [ ${fhr} -le ${fhmax} ]; do
       fhr2=$(printf %02i ${fhr})
       fhr3=$(printf %03i ${fhr})
       fname=${RUN}.t${cyc}z.sfluxgrbf${fhr3}.grib2
       fileout=${ARCDIR}/pgbq${fhr2}.${RUN}.${PDY}${cyc}.grib2
       ${WGRIB2} "${fname}" -match "(:PRATE:surface:)|(:TMP:2 m above ground:)" -grib "${fileout}"
       (( fhr = ${fhr} + 6 ))
    done
    export OMP_NUM_THREADS=${nthreads_env} # revert to threads set in env
fi


###############################################################
echo
echo "=============== START TO RUN MOS ==============="
if [ ${RUNMOS} = "YES" -a ${CDUMP} = "gfs" ]; then
    ${RUNGFSMOSSH} "${PDY}${cyc}"
fi


###############################################################
echo
echo "=============== START TO RUN RADMON DATA EXTRACTION ==============="

if [[ "${VRFYRAD}" == "YES" && "${CDUMP}" == "${CDFNL}" && "${PDY}${cyc}" != "${SDATE}" ]]; then

    export EXP=${PSLOT}
    export TANKverf_rad="${TANKverf}/stats/${PSLOT}/${RUN}.${PDY}/${cyc}"
    export TANKverf_radM1="${TANKverf}/stats/${PSLOT}/${RUN}.${PDYm1c}/${pcyc}"
    export MY_MACHINE=${machine}

    ${VRFYRADSH}

fi


###############################################################
echo
echo "=============== START TO RUN OZMON DATA EXTRACTION ==============="
if [[ "${VRFYOZN}" == "YES" && "${CDUMP}" == "${CDFNL}" && "${PDY}${cyc}" != "${SDATE}" ]]; then

    export EXP=${PSLOT}
    export TANKverf_ozn="${TANKverf_ozn}/stats/${PSLOT}/${RUN}.${PDY}/${cyc}"
    export TANKverf_oznM1="${TANKverf_ozn}/stats/${PSLOT}/${RUN}.${PDYm1c}/${pcyc}"
    export MY_MACHINE=${machine}

    ${VRFYOZNSH}

fi


###############################################################
echo
echo "=============== START TO RUN MINMON ==============="
if [[ "${VRFYMINMON}" == "YES" && "${PDY}${cyc}" != "${SDATE}" ]]; then

    export M_TANKverfM0="${M_TANKverf}/stats/${PSLOT}/${RUN}.${PDY}/${cyc}"
    export M_TANKverfM1="${M_TANKverf}/stats/${PSLOT}/${RUN}.${PDYm1c}/${pcyc}"
    export MY_MACHINE=${machine}

    ${VRFYMINSH}

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
