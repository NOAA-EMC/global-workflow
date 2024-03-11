#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

##############################################
# Begin JOB SPECIFIC work
##############################################
export n=$((10#${ENSGRP}))
export CDUMP_ENKF="${EUPD_CYC:-"gdas"}"

# ICS are restarts and always lag INC by $assim_freq hours.
EARCINC_CYC=${ARCH_CYC}
EARCICS_CYC=$((ARCH_CYC-assim_freq))
if [ "${EARCICS_CYC}" -lt 0 ]; then
  EARCICS_CYC=$((EARCICS_CYC+24))
fi

"${USHgfs}/hpssarch_gen.sh" "${RUN}"
status=$?
if [ "${status}" -ne 0 ]; then
   echo "${USHgfs}/hpssarch_gen.sh ${RUN} failed, ABORT!"
   exit "${status}"
fi

cd "${ROTDIR}" || exit 2

source "${USHgfs}/file_utils.sh"

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

     ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_grp${ENSGRP}.tar" $(cat "${DATA}/${RUN}_grp${n}.txt")
     status=$?
     if [ "${status}" -ne 0 ] && [ "${PDY}${cyc}" -ge "${firstday}" ]; then
         echo "FATAL ERROR: ${TARCMD} ${PDY}${cyc} ${RUN}_grp${ENSGRP}.tar failed"
         exit "${status}"
     fi

     if [ "${SAVEWARMICA}" = "YES" ] && [ "${cyc}" -eq "${EARCINC_CYC}" ]; then
       ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_restarta_grp${ENSGRP}.tar" $(cat "${DATA}/${RUN}_restarta_grp${n}.txt")
       status=$?
       if [ "${status}" -ne 0 ]; then
           echo "FATAL ERROR: ${TARCMD} ${PDY}${cyc} ${RUN}_restarta_grp${ENSGRP}.tar failed"
           exit "${status}"
       fi
     fi

     if [ "${SAVEWARMICB}" = "YES" ] && [ "${cyc}" -eq "${EARCICS_CYC}" ]; then
       ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_restartb_grp${ENSGRP}.tar" $(cat "${DATA}/${RUN}_restartb_grp${n}.txt")
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
        # Check if the tarball will have rstprod in it
        has_rstprod="NO"
        while IFS= read -r file; do
            if [[ -f ${file} ]]; then
                group=$( stat -c "%G" "${file}" )
                if [[ "${group}" == "rstprod" ]]; then
                    has_rstprod="YES"
                    break
                fi
            fi
        done < "${DATA}/${RUN}.txt"

        # Create the tarball
        tar_fl=${ATARDIR}/${PDY}${cyc}/${RUN}.tar
        ${TARCMD} -P -cvf "${tar_fl}" $(cat "${DATA}/${RUN}.txt")
        status=$?
        if [[ "${status}" -ne 0 ]]; then
            echo "FATAL ERROR: Tarring of ${tar_fl} failed"
            exit "${status}"
        fi

        # If rstprod was found, change the group of the tarball
        if [[ "${has_rstprod}" == "YES" ]]; then
            ${HSICMD} chgrp rstprod "${tar_fl}"
            stat_chgrp=$?
            ${HSICMD} chmod 640 "${tar_fl}"
            stat_chgrp=$((stat_chgrp+$?))
            if [[ "${stat_chgrp}" -gt 0 ]]; then
                echo "FATAL ERROR: Unable to properly restrict ${tar_fl}!"
                echo "Attempting to delete ${tar_fl}"
                ${HSICMD} rm "${tar_fl}"
                echo "Please verify that ${tar_fl} was deleted!"
                exit "${stat_chgrp}"
            fi
        fi

        # For safety, test if the htar/tar command failed only after changing groups
        if (( status != 0 && ${PDY}${cyc} >= firstday )); then
            echo "FATAL ERROR: ${TARCMD} ${tar_fl} failed"
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
