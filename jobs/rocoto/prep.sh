#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################
# Source relevant configs
configs="base prep"
for config in ${configs}; do
    . ${EXPDIR}/config.${config}
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

###############################################################
# Source machine runtime environment
. ${BASE_ENV}/${machine}.env prep
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################
# Set script and dependency variables
export COMPONENT=${COMPONENT:-atmos}
export OPREFIX="${CDUMP}.t${cyc}z."
export COMOUT="${ROTDIR}/${CDUMP}.${PDY}/${cyc}/${COMPONENT}"
export MAKE_PREPBUFR=${MAKE_PREPBUFR:-"YES"}
[[ ! -d ${COMOUT} ]] && mkdir -p ${COMOUT}
[[ ! -d ${COMIN_OBS} ]] && mkdir -p ${COMIN_OBS}

###############################################################
# If ROTDIR_DUMP=YES, copy dump files to rotdir
if [[ ${ROTDIR_DUMP} = "YES" ]]; then
   ${HOMEgfs}/ush/getdump.sh "${CDATE}" "${CDUMP}" "${DMPDIR}/${CDUMP}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}" "${COMIN_OBS}"
   status=$?
   [[ ${status} -ne 0 ]] && exit ${status}

#  Ensure previous cycle gdas dumps are available (used by cycle & downstream)
   GDATE=$(${NDATE} -${assim_freq} ${CDATE})
   gPDY=$(echo ${GDATE} | cut -c1-8)
   gcyc=$(echo ${GDATE} | cut -c9-10)
   GDUMP=gdas
   gCOMOBS="${ROTDIR}/${GDUMP}.${gPDY}/${gcyc}/obs"
   if [[ ! -s ${gCOMOBS}/${GDUMP}.t${gcyc}z.updated.status.tm00.bufr_d ]]; then
     ${HOMEgfs}/ush/getdump.sh "${GDATE}" "${GDUMP}" "${DMPDIR}/${GDUMP}${DUMP_SUFFIX}.${gPDY}/${gcyc}/${COMPONENT}" "${gCOMOBS}"
     status=$?
     [[ ${status} -ne 0 ]] && exit ${status}
   fi
   # exception handling to ensure no dead link
   [[ $(find ${COMIN_OBS} -xtype l | wc -l) -ge 1 ]] && exit 9
   [[ $(find ${gCOMOBS} -xtype l | wc -l) -ge 1 ]] && exit 9
fi


###############################################################

###############################################################
# For running real-time parallels, execute tropcy_qc and
# copy files from operational syndata directory to a local directory.
# Otherwise, copy existing tcvital data from globaldump.

if [[ ${PROCESS_TROPCY} = "YES" ]]; then

    export COMINsyn=${COMINsyn:-$(compath.py gfs/prod/syndat)}
    if [[ ${RUN_ENVIR} != "nco" ]]; then
        export ARCHSYND=${ROTDIR}/syndat
        if [[ ! -d ${ARCHSYND} ]]; then mkdir -p ${ARCHSYND}; fi
        if [[ ! -s ${ARCHSYND}/syndat_akavit ]]; then
            for file in syndat_akavit syndat_dateck syndat_stmcat.scr syndat_stmcat syndat_sthisto syndat_sthista ; do
                cp ${COMINsyn}/${file} ${ARCHSYND}/.
            done
        fi
    fi

    [[ ${ROTDIR_DUMP} = "YES" ]] && rm ${COMOUT}${CDUMP}.t${cyc}z.syndata.tcvitals.tm00

    ${HOMEgfs}/jobs/JGLOBAL_ATMOS_TROPCY_QC_RELOC
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}

else
    [[ ${ROTDIR_DUMP} = "NO" ]] && cp ${DMPDIR}/${CDUMP}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}/${CDUMP}.t${cyc}z.syndata.tcvitals.tm00 ${COMOUT}/
fi

# Will modify the new location later in the next PR to address issue 1198
if [[ ${ROTDIR_DUMP} = "YES" ]]; then
   mv ${COMIN_OBS}/*syndata.tcvitals.tm00 ${COMOUT}
   mv ${COMIN_OBS}/*snogrb_t1534.3072.1536 ${COMOUT}
   mv ${COMIN_OBS}/*seaice.5min.blend.grb ${COMOUT}
fi

###############################################################
# Generate prepbufr files from dumps or copy from OPS
if [[ ${MAKE_PREPBUFR} = "YES" ]]; then
    if [[ ${ROTDIR_DUMP} = "YES" ]]; then
        rm -f ${COMIN_OBS}/${OPREFIX}prepbufr
        rm -f ${COMIN_OBS}/${OPREFIX}prepbufr.acft_profiles
        rm -f ${COMIN_OBS}/${OPREFIX}nsstbufr
    fi

    export job="j${CDUMP}_prep_${cyc}"
    export DATAROOT="${RUNDIR}/${CDATE}/${CDUMP}/prepbufr"
    #export COMIN=${COMIN:-$ROTDIR/$CDUMP.$PDY/$cyc/$COMPONENT}
    #export COMIN=${COMIN:-$ROTDIR}
    export COMIN=${COMIN_OBS}
    export COMINgdas=${COMINgdas:-${ROTDIR}/gdas.${PDY}/${cyc}/${COMPONENT}}
    export COMINgfs=${COMINgfs:-${ROTDIR}/gfs.${PDY}/${cyc}/${COMPONENT}}
    export COMOUT=${COMIN_OBS}
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then
        COMIN_OBS=${COMIN_OBS:-${DMPDIR}/${CDUMP}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}}
        export COMSP=${COMSP:-${COMIN_OBS}/${CDUMP}.t${cyc}z.}
    else
        export COMSP=${COMSP:-${COMIN_OBS}/${CDUMP}.t${cyc}z.}
    fi

    # Disable creating NSSTBUFR if desired, copy from DMPDIR instead
    if [[ ${MAKE_NSSTBUFR:-"NO"} = "NO" ]]; then
        export MAKE_NSSTBUFR="NO"
    fi

    ${HOMEobsproc}/jobs/JOBSPROC_GLOBAL_PREP
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}

    # If creating NSSTBUFR was disabled, copy from DMPDIR if appropriate.
    if [[ ${MAKE_NSSTBUFR:-"NO"} = "NO" ]]; then
        [[ ${DONST} = "YES" ]] && ${NCP} ${DMPDIR}/${CDUMP}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}/${OPREFIX}nsstbufr ${COMIN_OBS}/${OPREFIX}nsstbufr
    fi

else
    if [[ ${ROTDIR_DUMP} = "NO" ]]; then
        ${NCP} ${DMPDIR}/${CDUMP}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}/${OPREFIX}prepbufr               ${COMIN_OBS}/${OPREFIX}prepbufr
        ${NCP} ${DMPDIR}/${CDUMP}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}/${OPREFIX}prepbufr.acft_profiles ${COMIN_OBS}/${OPREFIX}prepbufr.acft_profiles
        [[ ${DONST} = "YES" ]] && ${NCP} ${DMPDIR}/${CDUMP}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}/${OPREFIX}nsstbufr ${COMIN_OBS}/${OPREFIX}nsstbufr
    fi
fi

################################################################################
# Exit out cleanly


exit 0
