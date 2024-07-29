#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

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
GDATE=$(${NDATE} -"${assim_freq}" "${PDY}${cyc}")
# shellcheck disable=
gPDY=${GDATE:0:8}
gcyc=${GDATE:8:2}
GDUMP="gdas"

export OPREFIX="${RUN_local}.t${cyc}z."

YMD=${PDY} HH=${cyc} DUMP=${RUN_local} declare_from_tmpl -rx COM_OBS COM_OBSDMP

RUN=${GDUMP} DUMP=${GDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl -rx \
    COM_OBS_PREV:COM_OBS_TMPL \
    COM_OBSDMP_PREV:COM_OBSDMP_TMPL

export MAKE_PREPBUFR=${MAKE_PREPBUFR:-"YES"}
if [[ ! -d "${COM_OBS}" ]]; then mkdir -p "${COM_OBS}"; fi

###############################################################
# If ROTDIR_DUMP=YES, copy dump files to rotdir
if [[ ${ROTDIR_DUMP} = "YES" ]]; then
   "${HOMEgfs}/ush/getdump.sh" "${PDY}${cyc}" "${RUN_local}" "${COM_OBSDMP}" "${COM_OBS}"
   status=$?
   [[ ${status} -ne 0 ]] && exit ${status}

   #  Ensure previous cycle gdas dumps are available (used by cycle & downstream)
   if [[ ! -s "${COM_OBS_PREV}/${GDUMP}.t${gcyc}z.updated.status.tm00.bufr_d" ]]; then
     "${HOMEgfs}/ush/getdump.sh" "${GDATE}" "${GDUMP}" "${COM_OBSDMP_PREV}" "${COM_OBS_PREV}"
     status=$?
     [[ ${status} -ne 0 ]] && exit ${status}
   fi
   # exception handling to ensure no dead link
   [[ $(find ${COM_OBS} -xtype l | wc -l) -ge 1 ]] && exit 9
   [[ $(find ${COM_OBS_PREV} -xtype l | wc -l) -ge 1 ]] && exit 9
fi


###############################################################

###############################################################
# For running real-time parallels, execute tropcy_qc and
# copy files from operational syndata directory to a local directory.
# Otherwise, copy existing tcvital data from globaldump.

if [[ ${PROCESS_TROPCY} = "YES" ]]; then

    export COMINsyn=${COMINsyn:-$(compath.py gfs/prod/syndat)}
    export ARCHSYND=${ROTDIR}/syndat
    if [[ ! -d ${ARCHSYND} ]]; then mkdir -p ${ARCHSYND}; fi
    if [[ ! -s ${ARCHSYND}/syndat_akavit ]]; then
        for file in syndat_akavit syndat_dateck syndat_stmcat.scr syndat_stmcat syndat_sthisto syndat_sthista ; do
            cp ${COMINsyn}/${file} ${ARCHSYND}/.
        done
    fi

    if [[ ${ROTDIR_DUMP} = "YES" ]]; then rm "${COM_OBS}/${RUN_local}.t${cyc}z.syndata.tcvitals.tm00"; fi

    "${HOMEgfs}/jobs/JGLOBAL_ATMOS_TROPCY_QC_RELOC"
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}

else
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then cp "${COM_OBSDMP}/${RUN_local}.t${cyc}z.syndata.tcvitals.tm00" "${COM_OBS}/"; fi
fi


###############################################################
# Generate prepbufr files from dumps or copy from OPS
if [[ ${MAKE_PREPBUFR} = "YES" ]]; then
    if [[ ${ROTDIR_DUMP} = "YES" ]]; then
        rm -f "${COM_OBS}/${OPREFIX}prepbufr"
        rm -f "${COM_OBS}/${OPREFIX}prepbufr.acft_profiles"
        rm -f "${COM_OBS}/${OPREFIX}nsstbufr"
    fi

    export job="j${RUN_local}_prep_${cyc}"
    export COMIN=${COM_OBS}
    export COMOUT=${COM_OBS}
    RUN="gdas" YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COMINgdas:COM_ATMOS_HISTORY_TMPL
    RUN="gfs" YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COMINgfs:COM_ATMOS_HISTORY_TMPL
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then
        export COMSP=${COMSP:-"${COM_OBSDMP}/${RUN_local}.t${cyc}z."}
    else
        export COMSP=${COMSP:-"${COM_OBS}/${RUN_local}.t${cyc}z."}
    fi
    export COMSP=${COMSP:-${COMIN_OBS}/${RUN_local}.t${cyc}z.}

    # Disable creating NSSTBUFR if desired, copy from DMPDIR instead
    if [[ ${MAKE_NSSTBUFR:-"NO"} = "NO" ]]; then
        export MAKE_NSSTBUFR="NO"
    fi

    "${HOMEobsproc}/jobs/JOBSPROC_GLOBAL_PREP"
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}

    # If creating NSSTBUFR was disabled, copy from DMPDIR if appropriate.
    if [[ ${MAKE_NSSTBUFR:-"NO"} = "NO" ]]; then
        if [[ ${DONST} = "YES" ]]; then ${NCP} "${COM_OBSDMP}/${OPREFIX}nsstbufr" "${COM_OBS}/${OPREFIX}nsstbufr"; fi
    fi

else
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then
        ${NCP} "${COM_OBSDMP}/${OPREFIX}prepbufr"               "${COM_OBS}/${OPREFIX}prepbufr"
        ${NCP} "${COM_OBSDMP}/${OPREFIX}prepbufr.acft_profiles" "${COM_OBS}/${OPREFIX}prepbufr.acft_profiles"
        if [[ ${DONST} = "YES" ]]; then ${NCP} "${COM_OBSDMP}/${OPREFIX}nsstbufr" "${COM_OBS}/${OPREFIX}nsstbufr"; fi
    fi
fi

################################################################################
# Exit out cleanly


exit 0
