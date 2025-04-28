#! /usr/bin/env bash

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

###############################################################
export job="prep"
export jobid="${job}.$$"
source "${HOMEgfs}/ush/jjob_header.sh" -e "prep" -c "base prep"

# Strip 'enkf' from RUN for pulling data
RUN_local="${RUN/enkf}"

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

YMD=${PDY} HH=${cyc} DUMP=${RUN_local} declare_from_tmpl -rx \
    COMIN_OBS:COM_OBS_TMPL \
    COMOUT_OBS:COM_OBS_TMPL \
    COMIN_OBSDMP:COM_OBSDMP_TMPL

RUN=${GDUMP} DUMP=${GDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl -rx \
    COMIN_OBS_PREV:COM_OBS_TMPL \
    COMIN_OBSDMP_PREV:COM_OBSDMP_TMPL

export MAKE_PREPBUFR=${MAKE_PREPBUFR:-"YES"}
if [[ ! -d "${COMOUT_OBS}" ]]; then mkdir -p "${COMOUT_OBS}"; fi

###############################################################
# If ROTDIR_DUMP=YES, copy dump files to rotdir
if [[ ${ROTDIR_DUMP} = "YES" ]]; then
   "${HOMEgfs}/ush/getdump.sh" "${PDY}${cyc}" "${RUN_local}" "${COMIN_OBSDMP}" "${COMOUT_OBS}"
   status=$?
   if [[ ${status} -ne 0 ]]; then
       exit "${status}"
   fi

   #  Ensure previous cycle gdas dumps are available (used by cycle & downstream)
   if [[ ! -s "${COMIN_OBS_PREV}/${GDUMP}.t${gcyc}z.updated.status.tm00.bufr_d" ]]; then
     "${HOMEgfs}/ush/getdump.sh" "${GDATE}" "${GDUMP}" "${COMIN_OBSDMP_PREV}" "${COMIN_OBS_PREV}"
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
   if [[ $(find "${COMIN_OBS_PREV}" -xtype l | wc -l) -ge 1 ]]; then
       exit 9
   fi
fi


###############################################################

###############################################################
# For running real-time parallels, execute tropcy_qc and
# copy files from operational syndata directory to a local directory.
# Otherwise, copy existing tcvital data from globaldump.

if [[ ${PROCESS_TROPCY} = "YES" ]]; then

    export COMINsyn=${COMINsyn:-$(compath.py gfs/prod/syndat)}
    export ARCHSYND=${ROTDIR}/syndat
    if [[ ! -d ${ARCHSYND} ]]; then mkdir -p "${ARCHSYND}"; fi
    if [[ ! -s ${ARCHSYND}/syndat_akavit ]]; then
        for file in syndat_akavit syndat_dateck syndat_stmcat.scr syndat_stmcat syndat_sthisto syndat_sthista ; do
            cp "${COMINsyn}/${file}" "${ARCHSYND}"/.
        done
    fi

    if [[ ${ROTDIR_DUMP} = "YES" ]]; then rm "${COMOUT_OBS}/${RUN_local}.t${cyc}z.syndata.tcvitals.tm00"; fi

    "${HOMEgfs}/jobs/JGLOBAL_ATMOS_TROPCY_QC_RELOC"
    status=$?
    if [[ ${status} -ne 0 ]]; then
        exit "${status}"
    fi

else
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then cp "${COMIN_OBSDMP}/${RUN_local}.t${cyc}z.syndata.tcvitals.tm00" "${COMOUT_OBS}/"; fi
fi


###############################################################
# Generate prepbufr files from dumps or copy from OPS
if [[ ${MAKE_PREPBUFR} = "YES" ]]; then
    if [[ ${ROTDIR_DUMP} = "YES" ]]; then
        rm -f "${COMOUT_OBS}/${OPREFIX}prepbufr"
        rm -f "${COMOUT_OBS}/${OPREFIX}prepbufr.acft_profiles"
        rm -f "${COMOUT_OBS}/${OPREFIX}nsstbufr"
    fi

    export job="j${RUN_local}_prep_${cyc}"
    export COMIN=${COMIN_OBS}
    export COMOUT=${COMOUT_OBS}
    RUN="gdas" YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COMINgdas:COM_ATMOS_HISTORY_TMPL
    RUN="gfs" YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COMINgfs:COM_ATMOS_HISTORY_TMPL
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then
        export COMSP=${COMSP:-"${COMIN_OBSDMP}/${RUN_local}.t${cyc}z."}
    else
        export COMSP=${COMSP:-"${COMIN_OBS}/${RUN_local}.t${cyc}z."}
    fi
    export COMSP=${COMSP:-${COMIN_OBS}/${RUN_local}.t${cyc}z.}

    # Disable creating NSSTBUFR if desired, copy from DMPDIR instead
    if [[ ${MAKE_NSSTBUFR:-"NO"} = "NO" ]]; then
        export MAKE_NSSTBUFR="NO"
    fi

    # Do not fail on external errors
    set +eu
    "${HOMEobsproc}/jobs/JOBSPROC_GLOBAL_PREP" && true
    err=$?
    if [[ ${err} -ne 0 ]]; then
       echo "FATAL ERROR: Global prep job failed!"
       exit 1
    fi
    set_strict

    # If creating NSSTBUFR was disabled, copy from DMPDIR if appropriate.
    if [[ ${MAKE_NSSTBUFR:-"NO"} = "NO" ]]; then
        if [[ ${DONST} = "YES" ]]; then ${NCP} "${COMIN_OBSDMP}/${OPREFIX}nsstbufr" "${COMOUT_OBS}/${OPREFIX}nsstbufr"; fi
    fi

else
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then
        ${NCP} "${COMIN_OBSDMP}/${OPREFIX}prepbufr"               "${COMOUT_OBS}/${OPREFIX}prepbufr"
        ${NCP} "${COMIN_OBSDMP}/${OPREFIX}prepbufr.acft_profiles" "${COMOUT_OBS}/${OPREFIX}prepbufr.acft_profiles"
        if [[ ${DONST} = "YES" ]]; then ${NCP} "${COMIN_OBSDMP}/${OPREFIX}nsstbufr" "${COMOUT_OBS}/${OPREFIX}nsstbufr"; fi
    fi
fi

################################################################################
# Exit out cleanly


exit 0
