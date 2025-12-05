#! /usr/bin/env bash

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if [[ ${status} -ne 0 ]]; then
    err_exit "${status}"
fi

###############################################################
export job="prep"
export jobid="${job}.$$"
source "${HOMEgfs}/ush/jjob_header.sh" -e "prep" -c "base prep"

# Strip 'enkf' from RUN for pulling data
RUN_local="${RUN/enkf/}"

###############################################################
# Set script and dependency variables
# Ignore possible spelling error (nothing is misspelled)
# shellcheck disable=SC2153
GDATE=$(date --utc -d "${PDY} ${cyc} - ${assim_freq} hours" +%Y%m%d%H)
# shellcheck disable=
gPDY=${GDATE:0:8}
gcyc=${GDATE:8:2}
GDUMP="gdas"

export OPREFIX="${RUN_local}.t${cyc}z."

RUN=${RUN_local} YMD=${PDY} HH=${cyc} declare_from_tmpl -rx \
    COMIN_OBS:COM_OBS_TMPL \
    COMOUT_OBS:COM_OBS_TMPL \
    COMINobsproc:COM_OBSPROC_TMPL \
    COMINobsforge:COM_OBSFORGE_TMPL \
    COMIN_TCVITAL:COM_TCVITAL_TMPL \
    COMOUT_ATMOS_ANALYSIS:COM_ATMOS_ANALYSIS_TMPL

RUN=${GDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl -rx \
    COMOUT_OBS_PREV:COM_OBS_TMPL \
    COMINobsproc_PREV:COM_OBSPROC_TMPL \
    COMOUT_ATMOS_ANALYSIS_PREV:COM_ATMOS_ANALYSIS_TMPL

mkdir -p "${COMOUT_OBS}"

###############################################################
# Copy IODA files to ROTDIR
if [[ ${USE_IODADIR:-"NO"} == "YES" ]]; then
    "${HOMEgfs}/ush/getioda.sh" "${PDY}" "${cyc}" "${RUN_local}" "${COMINobsforge}" "${COMOUT_OBS}"
    status=$?
    if [[ ${status} -ne 0 ]]; then
        exit "${status}"
    fi
fi

if [[ "${RUN_local}" == "gcdas" ]]; then
    echo "GCDAS only needs IODA files; exiting prep.sh"
    exit 0
fi

# Copy dump files to ROTDIR
"${HOMEgfs}/ush/getdump.sh" "${PDY}" "${cyc}" "${RUN_local}" "${COMINobsproc}" "${COMOUT_OBS}"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

#  Ensure previous cycle gdas dumps are available (used by cycle & downstream)
if [[ ! -s "${COMINobsproc_PREV}/${GDUMP}.t${gcyc}z.updated.status.tm00.bufr_d" ]]; then
    "${HOMEgfs}/ush/getdump.sh" "${gPDY}" "${gcyc}" "${GDUMP}" "${COMINobsproc_PREV}" "${COMOUT_OBS_PREV}"
    status=$?
    if [[ ${status} -ne 0 ]]; then
        exit "${status}"
    fi
fi
# exception handling to ensure no dead link
# shellcheck disable=SC2312
if [[ $(find "${COMOUT_OBS}" -xtype l | wc -l) -ge 1 ]]; then
    exit 9
fi
# shellcheck disable=SC2312
if [[ $(find "${COMINobsproc_PREV}" -xtype l | wc -l) -ge 1 ]]; then
    exit 9
fi

###############################################################

###############################################################
# For running real-time parallels, execute tropcy_qc and
# copy files from operational syndata directory to a local directory.
# Otherwise, copy existing tcvital data from globaldump.

if [[ ${PROCESS_TROPCY} == "YES" ]]; then

    export ARCHSYND=${ROTDIR}/syndat
    mkdir -p "${ARCHSYND}"
    if [[ ! -s ${ARCHSYND}/syndat_akavit ]]; then
        for file in syndat_akavit syndat_dateck syndat_stmcat.scr syndat_stmcat syndat_sthisto syndat_sthista; do
            cpreq "${COMINsyn}/${file}" "${ARCHSYND}"/.
        done
    fi

    rm -f "${COMOUT_OBS}/${RUN_local}.t${cyc}z.syndata.tcvitals.tm00"

    "${HOMEgfs}/dev/jobs/JGLOBAL_ATMOS_TROPCY_QC_RELOC"
    status=$?
    if [[ ${status} -ne 0 ]]; then
        exit "${status}"
    fi

else
    cpfs "${COMINobsproc}/${RUN_local}.t${cyc}z.syndata.tcvitals.tm00" "${COMOUT_OBS}/"
fi

###############################################################
# If requested, copy bias correction files from source or stait to analysis directories
# TODO: remove this when JEDI ATM can cycle bias correction coefficents
if [[ ${RUN} == "gdas" && ${COPY_BIASCOR_SOURCE:-"NO"} == "YES" ]]; then
    for file in abias abias_pc abias_air; do
        if [[ -s "${SOURCE_BIASCOR}/${file}.${GDUMP}.${gPDY}${gcyc}" ]]; then
            cpreq "${SOURCE_BIASCOR}/${file}.${GDUMP}.${gPDY}${gcyc}" "${COMOUT_ATMOS_ANALYSIS_PREV}/${GDUMP}.t${gcyc}z.${file}"
            cpreq "${SOURCE_BIASCOR}/${file}.${GDUMP}.${gPDY}${gcyc}" "${COMOUT_ATMOS_ANALYSIS_PREV}/${GDUMP}.t${gcyc}z.${file}.txt"
        fi
    done
fi
if [[ ${RUN} == "gdas" && ${COPY_BIASCOR_STATIC:-"NO"} == "YES" ]]; then
    for file in abias abias_pc abias_air; do
        if [[ -s "${COMOUT_ATMOS_ANALYSIS_PREV}/${GDUMP}.t${gcyc}z.${file}.txt" ]]; then
            mkdir -p "${COMOUT_ATMOS_ANALYSIS}"
            cpreq "${COMOUT_ATMOS_ANALYSIS_PREV}/${GDUMP}.t${gcyc}z.${file}.txt" "${COMOUT_ATMOS_ANALYSIS}/${GDUMP}.t${cyc}z.${file}.txt"
        fi
    done
fi

###############################################################
# Generate prepbufr files from dumps and prior gdas guess
rm -f "${COMOUT_OBS}/${OPREFIX}prepbufr"
rm -f "${COMOUT_OBS}/${OPREFIX}prepbufr.acft_profiles"
rm -f "${COMOUT_OBS}/${OPREFIX}nsstbufr"

RUN="gdas" YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COMIN_ATMOS_HISTORY_GDAS:COM_ATMOS_HISTORY_TMPL
RUN="gfs" YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COMIN_ATMOS_HISTORY_GFS:COM_ATMOS_HISTORY_TMPL

export job="j${RUN_local}_prep_${cyc}"

#TODO: Update external packages (obsproc/prepobs) to use COMIN[OUT]_*
export COMINtcvital=${COMIN_TCVITAL}
export COMIN=${COMIN_OBS}
export COMOUT=${COMOUT_OBS}
export COMINgdas=${COMIN_ATMOS_HISTORY_GDAS}
export COMINgfs=${COMIN_ATMOS_HISTORY_GFS}

export COMSP=${COMSP:-"${COMIN_OBS}/${RUN_local}.t${cyc}z."}

# Create or Copy prepbufr, prepbufr.acft_profiles, nsstbufr files
# Do not fail on external errors
if [[ ${MAKE_PREPBUFR:-"YES"} == "YES" ]]; then
    set +eu
    "${HOMEobsproc}/jobs/JOBSPROC_GLOBAL_PREP" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "JOBSPROC_GLOBAL_PREP job failed, ABORT!"
    fi
else
    if [[ ${USE_PREPBUFR_FROM_OPS:-"YES"} == "YES" ]]; then
        # If USE_PREPBUFR_FROM_OPS is set, copy prepbufr from COMINobsproc
        PREPBUFR_DIR="${COMINobsproc}"
    else
        # If PREPBUFR_DIR is not set, exit out with an error
        if [[ -z "${PREPBUFR_DIR}" ]]; then
            export err=1
            err_exit "PREPBUFR_DIR is not set!"
        fi

    fi
    cpreq "${PREPBUFR_DIR}/${OPREFIX}prepbufr" "${COMOUT_OBS}/${OPREFIX}prepbufr"
    cpreq "${PREPBUFR_DIR}/${OPREFIX}prepbufr.acft_profiles" "${COMOUT_OBS}/${OPREFIX}prepbufr.acft_profiles"
    if [[ ${DONST} == "YES" ]]; then
        cpreq "${PREPBUFR_DIR}/${OPREFIX}nsstbufr" "${COMOUT_OBS}/${OPREFIX}nsstbufr"
    fi
fi

# Check if prepbufr, etc files were copied to COMOUT_OBS
files="prepbufr prepbufr.acft_profiles"
if [[ ${DONST} == "YES" ]]; then
    files="${files} nsstbufr"
fi
err=0
for file in ${files}; do
    if [[ ! -f "${COMOUT_OBS}/${OPREFIX}${file}" ]]; then
        err=1
        echo "Failed to obtain/create ${file}, ABORT!"
    fi
done
export err
if [[ ${err} -ne 0 ]]; then
    err_exit "Failed to obtain/create ${files}, ABORT!"
fi

################################################################################
# If requested, create radiance bias correction files for JEDI
if [[ ${RUN} == "gdas" && ${CONVERT_BIASCOR:-"NO"} == "YES" ]]; then
    cd "${DATAROOT}" || true
    "${HOMEgfs}/ush/gsi_satbias2ioda_all.sh"
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "gsi_satbias2ioda failed, ABORT!"
    fi

    # Remove temporary working directory
    cd "${DATAROOT}" || true
    if [[ "${KEEPDATA}" == "NO" ]]; then
        rm -rf "${DATA}"
    fi
fi

################################################################################
# Exit out cleanly

exit 0
