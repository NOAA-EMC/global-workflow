#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

##############################################
# Begin JOB SPECIFIC work
##############################################
export n=$((10#${ENSGRP}))
export CDUMP_ENKF="${EUPD_CYC:-"gdas"}"

export ARCH_LIST="${COM_TOP}/earc${ENSGRP}"

# ICS are restarts and always lag INC by $assim_freq hours.
EARCINC_CYC=${ARCH_CYC}
EARCICS_CYC=$((ARCH_CYC-assim_freq))
if [ "${EARCICS_CYC}" -lt 0 ]; then
  EARCICS_CYC=$((EARCICS_CYC+24))
fi

[[ -d ${ARCH_LIST} ]] && rm -rf "${ARCH_LIST}"
mkdir -p "${ARCH_LIST}"
cd "${ARCH_LIST}" || exit 2

"${HOMEgfs}/ush/hpssarch_gen.sh" "${RUN}"
status=$?
if [ "${status}" -ne 0 ]; then
   echo "${HOMEgfs}/ush/hpssarch_gen.sh ${RUN} failed, ABORT!"
   exit "${status}"
fi

cd "${ROTDIR}" || exit 2

source "${HOMEgfs}/ush/file_utils.sh"

###################################################################
# ENSGRP > 0 archives a group of ensemble members
firstday=$(${NDATE} +24 "${SDATE}")
if (( 10#${ENSGRP} > 0 )) && [[ ${HPSSARCH} = "YES" || ${LOCALARCH} = "YES" ]]; then

#--set the archiving command and create local directories, if necessary
   TARCMD="htar"
   if [[ ${LOCALARCH} = "YES" ]]; then
       TARCMD="tar"
       if [[ ! -d "${ATARDIR}/${PDY}${cyc}" ]]; then mkdir -p "${ATARDIR}/${PDY}${cyc}"; fi
   fi

#--determine when to save ICs for warm start
   SAVEWARMICA="NO"
   SAVEWARMICB="NO"
   mm="${PDY:4:2}"
   dd="${PDY:6:2}"
   nday=$(( (10#${mm}-1)*30+10#${dd} ))
   mod=$((nday % ARCH_WARMICFREQ))
   if [ "${PDY}${cyc}" -eq "${firstday}" ] && [ "${cyc}" -eq "${EARCINC_CYC}" ]; then SAVEWARMICA="YES" ; fi
   if [ "${PDY}${cyc}" -eq "${firstday}" ] && [ "${cyc}" -eq "${EARCICS_CYC}" ]; then SAVEWARMICB="YES" ; fi
   if [ "${mod}" -eq 0 ] && [ "${cyc}" ] && [ "${EARCINC_CYC}" ]; then SAVEWARMICA="YES" ; fi
   if [ "${mod}" -eq 0 ] && [ "${cyc}" ] && [ "${EARCICS_CYC}" ]; then SAVEWARMICB="YES" ; fi

   if [ "${EARCICS_CYC}" -eq 18 ]; then
       nday1=$((nday+1))
       mod1=$((nday1 % ARCH_WARMICFREQ))
       if [ "${mod1}" -eq 0 ] && [ "${cyc}" -eq "${EARCICS_CYC}" ] ; then SAVEWARMICB="YES" ; fi
       if [ "${mod1}" -ne 0 ] && [ "${cyc}" -eq "${EARCICS_CYC}" ] ; then SAVEWARMICB="NO" ; fi
       if [ "${PDY}${cyc}" -eq "${SDATE}" ] && [ "${cyc}" -eq "${EARCICS_CYC}" ] ; then SAVEWARMICB="YES" ; fi
   fi

   if [ "${PDY}${cyc}" -gt "${SDATE}" ]; then # Don't run for first half cycle

     ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_grp${ENSGRP}.tar" $(cat "${ARCH_LIST}/${RUN}_grp${n}.txt")
     status=$?
     if [ "${status}" -ne 0 ] && [ "${PDY}${cyc}" -ge "${firstday}" ]; then
         echo "FATAL ERROR: ${TARCMD} ${PDY}${cyc} ${RUN}_grp${ENSGRP}.tar failed"
         exit "${status}"
     fi

     if [ "${SAVEWARMICA}" = "YES" ] && [ "${cyc}" -eq "${EARCINC_CYC}" ]; then
       ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_restarta_grp${ENSGRP}.tar" $(cat "${ARCH_LIST}/${RUN}_restarta_grp${n}.txt")
       status=$?
       if [ "${status}" -ne 0 ]; then
           echo "FATAL ERROR: ${TARCMD} ${PDY}${cyc} ${RUN}_restarta_grp${ENSGRP}.tar failed"
           exit "${status}"
       fi
     fi

     if [ "${SAVEWARMICB}" = "YES" ] && [ "${cyc}" -eq "${EARCICS_CYC}" ]; then
       ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_restartb_grp${ENSGRP}.tar" $(cat "${ARCH_LIST}/${RUN}_restartb_grp${n}.txt")
       status=$?
       if [ "${status}" -ne 0 ]; then
           echo "FATAL ERROR: ${TARCMD} ${PDY}${cyc} ${RUN}_restartb_grp${ENSGRP}.tar failed"
           exit "${status}"
       fi
     fi

   fi # CDATE>SDATE

fi


###################################################################
# ENSGRP 0 archives ensemble means and copy data to online archive
if [ "${ENSGRP}" -eq 0 ]; then

    if [[ ${HPSSARCH} = "YES" || ${LOCALARCH} = "YES" ]]; then

        #--set the archiving command and create local directories, if necessary
        TARCMD="htar"
        HSICMD="hsi"
        if [[ ${LOCALARCH} = "YES" ]]; then
            TARCMD="tar"
            HSICMD=""
            if [[ ! -d "${ATARDIR}/${PDY}${cyc}" ]]; then mkdir -p "${ATARDIR}/${PDY}${cyc}"; fi
        fi

        set +e
        ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}.tar" $(cat "${ARCH_LIST}/${RUN}.txt")
        status=$?
        ${HSICMD} chgrp rstprod "${ATARDIR}/${PDY}${cyc}/${RUN}.tar"
        ${HSICMD} chmod 640 "${ATARDIR}/${PDY}${cyc}/${RUN}.tar"
        if (( status != 0 && ${PDY}${cyc} >= firstday )); then
            echo "FATAL ERROR: ${TARCMD} ${PDY}${cyc} ${RUN}.tar failed"
            exit "${status}"
        fi
        set_strict
    fi

    #-- Archive online for verification and diagnostics
    [[ ! -d ${ARCDIR} ]] && mkdir -p "${ARCDIR}"
    cd "${ARCDIR}" || exit 2

    nb_copy "${COM_ATMOS_ANALYSIS_ENSSTAT}/${RUN}.t${cyc}z.enkfstat" \
        "enkfstat.${RUN}.${PDY}${cyc}"
    nb_copy "${COM_ATMOS_ANALYSIS_ENSSTAT}/${RUN}.t${cyc}z.gsistat.ensmean" \
        "gsistat.${RUN}.${PDY}${cyc}.ensmean"
fi

exit 0
