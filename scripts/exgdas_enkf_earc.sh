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


if [[ "${DELETE_COM_IN_ARCHIVE_JOB:-YES}" == NO ]] ; then
    exit 0
fi

###############################################################
# ENSGRP 0 also does clean-up
###############################################################
if [[ "${ENSGRP}" -eq 0 ]]; then
    function remove_files() {
        # TODO: move this to a new location
        local directory=$1
        shift
        if [[ ! -d ${directory} ]]; then
            echo "No directory ${directory} to remove files from, skiping"
            return
        fi
        local exclude_list=""
        if (($# > 0)); then
            exclude_list=$*
        fi
        local file_list
        declare -a file_list
        readarray -t file_list < <(find -L "${directory}" -type f)
        # echo "Number of files to remove before exclusions: ${#file_list[@]}"
        if (( ${#file_list[@]} == 0 )); then return; fi
        for exclude in ${exclude_list}; do
            echo "Excluding ${exclude}"
            declare -a file_list_old=("${file_list[@]}")
            readarray file_list < <(printf -- '%s\n' "${file_list_old[@]}" | grep -v "${exclude}")
            # echo "Number of files to remove after exclusion: ${#file_list[@]}"
            if (( ${#file_list[@]} == 0 )); then return; fi
        done
        # echo "Number of files to remove after exclusions: ${#file_list[@]}"

        for file in "${file_list[@]}"; do
            rm -f "${file}"
        done
        # Remove directory if empty
        rmdir "${directory}" || true
    }

    # Start start and end dates to remove
    GDATEEND=$(${NDATE} -"${RMOLDEND_ENKF:-24}"  "${PDY}${cyc}")
    GDATE=$(${NDATE} -"${RMOLDSTD_ENKF:-120}" "${PDY}${cyc}")

    while [ "${GDATE}" -le "${GDATEEND}" ]; do

        gPDY="${GDATE:0:8}"
        gcyc="${GDATE:8:2}"

        if [[ -d ${COM_TOP} ]]; then
            rocotolog="${EXPDIR}/logs/${GDATE}.log"
            if [[ -f "${rocotolog}" ]]; then
                set +e
                testend=$(tail -n 1 "${rocotolog}" | grep "This cycle is complete: Success")
                rc=$?
                set_strict
                if [ "${rc}" -eq 0 ]; then
                    case ${CDUMP} in
                        gdas)   nmem=${NMEM_ENKF};;
                        gfs)    nmem=${NMEM_EFCS};;
                        *)
                            echo "FATAL ERROR: Unknown CDUMP ${CDUMP} during cleanup"
                            exit 10
                            ;;
                    esac

                    readarray memlist< <(seq --format="mem%03g" 1 "${nmem}")
                    memlist+=("ensstat")

                    for mem in ${memlist}; do
                        # Atmos
                        exclude_list="f006.ens"
                        templates=$(compgen -A variable | grep 'COM_ATMOS_.*_TMPL')
                        for template in ${templates}; do
                            MEMDIR="${mem}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                            remove_files "${directory}" "${exclude_list[@]}"
                        done

                        # Wave
                        exclude_list=""
                        templates=$(compgen -A variable | grep 'COM_WAVE_.*_TMPL')
                        for template in ${templates}; do
                            MEMDIR="${mem}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                            remove_files "${directory}" "${exclude_list[@]}"
                        done

                        # Ocean
                        exclude_list=""
                        templates=$(compgen -A variable | grep 'COM_OCEAN_.*_TMPL')
                        for template in ${templates}; do
                            YMEMDIR="${mem}" MD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                            remove_files "${directory}" "${exclude_list[@]}"
                        done

                        # Ice
                        exclude_list=""
                        templates=$(compgen -A variable | grep 'COM_ICE_.*_TMPL')
                        for template in ${templates}; do
                            MEMDIR="${mem}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                            remove_files "${directory}" "${exclude_list[@]}"
                        done

                        # Aerosols (GOCART)
                        exclude_list=""
                        templates=$(compgen -A variable | grep 'COM_CHEM_.*_TMPL')
                        for template in ${templates}; do
                            MEMDIR="${mem}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                            remove_files "${directory}" "${exclude_list[@]}"
                        done

                        # Mediator
                        exclude_list=""
                        templates=$(compgen -A variable | grep 'COM_MED_.*_TMPL')
                        for template in ${templates}; do
                            MEMDIR="${mem}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                            remove_files "${directory}" "${exclude_list[@]}"
                        done
                    done
                fi
            fi
        fi

        # Remove any empty directories
        YMD=${gPDY} HH=${gcyc} generate_com target_dir:COM_TOP_TMPL
        target_dir="${ROTDIR:?}/${RUN}.${gPDY}/${gcyc}/"
        if [[ -d ${target_dir} ]]; then
            find "${target_dir}" -empty -type d -delete
        fi

        # Advance to next cycle
        GDATE=$(${NDATE} +"${assim_freq}" "${GDATE}")
    done
fi

# Remove enkf*.$rPDY for the older of GDATE or RDATE
GDATE=$(${NDATE} -"${RMOLDSTD_ENKF:-120}" "${PDY}${cyc}")
fhmax=${FHMAX_GFS}
RDATE=$(${NDATE} -"${fhmax}" "${PDY}${cyc}")
if [ "${GDATE}" -lt "${RDATE}" ]; then
    RDATE=${GDATE}
fi
rPDY=$(echo "${RDATE}" | cut -c1-8)
clist="enkfgdas enkfgfs"
for ctype in ${clist}; do
    COMIN="${ROTDIR}/${ctype}.${rPDY}"
    [[ -d ${COMIN} ]] && rm -rf "${COMIN}"
done

###############################################################


exit 0
