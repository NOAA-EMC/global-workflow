#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

##############################################
# Begin JOB SPECIFIC work
##############################################

# ICS are restarts and always lag INC by $assim_freq hours
ARCHINC_CYC=${ARCH_CYC}
ARCHICS_CYC=$((ARCH_CYC-assim_freq))
if [ "${ARCHICS_CYC}" -lt 0 ]; then
    ARCHICS_CYC=$((ARCHICS_CYC+24))
fi

# CURRENT CYCLE
APREFIX="${RUN}.t${cyc}z."

# Realtime parallels run GFS MOS on 1 day delay
# If realtime parallel, back up CDATE_MOS one day
CDATE_MOS=${PDY}${cyc}
if [ "${REALTIME}" = "YES" ]; then
    CDATE_MOS=$(${NDATE} -24 "${PDY}${cyc}")
fi
PDY_MOS="${CDATE_MOS:0:8}"

###############################################################
# Archive online for verification and diagnostics
###############################################################
# COMIN=${COMINatmos:-"${ROTDIR}/${RUN}.${PDY}/${cyc}/atmos"}
# cd "${COMIN}"

source "${HOMEgfs}/ush/file_utils.sh"

[[ ! -d ${ARCDIR} ]] && mkdir -p "${ARCDIR}"
nb_copy "${COM_ATMOS_ANALYSIS}/${APREFIX}gsistat" "${ARCDIR}/gsistat.${RUN}.${PDY}${cyc}"
nb_copy "${COM_ATMOS_GRIB_1p00}/${APREFIX}pgrb2.1p00.anl" "${ARCDIR}/pgbanl.${RUN}.${PDY}${cyc}.grib2"

# Archive 1 degree forecast GRIB2 files for verification
if [[ "${RUN}" == "gfs" ]]; then
    fhmax=${FHMAX_GFS}
    fhr=0
    while [ "${fhr}" -le "${fhmax}" ]; do
        fhr2=$(printf %02i "${fhr}")
        fhr3=$(printf %03i "${fhr}")
        nb_copy "${COM_ATMOS_GRIB_1p00}/${APREFIX}pgrb2.1p00.f${fhr3}" "${ARCDIR}/pgbf${fhr2}.${RUN}.${PDY}${cyc}.grib2"
        fhr=$((10#${fhr} + 10#${FHOUT_GFS} ))
    done
fi
if [[ "${RUN}" == "gdas" ]]; then
    flist="000 003 006 009"
    for fhr in ${flist}; do
        fname="${COM_ATMOS_GRIB_1p00}/${APREFIX}pgrb2.1p00.f${fhr}"
        # TODO Shouldn't the archived files also use three-digit tags?
        fhr2=$(printf %02i $((10#${fhr})))
        nb_copy "${fname}" "${ARCDIR}/pgbf${fhr2}.${RUN}.${PDY}${cyc}.grib2"
    done
fi

if [[ -s "${COM_ATMOS_TRACK}/avno.t${cyc}z.cyclone.trackatcfunix" ]]; then
    # shellcheck disable=2153
    PSLOT4=${PSLOT:0:4}
    # shellcheck disable=
    PSLOT4=${PSLOT4^^}
    sed "s:AVNO:${PSLOT4}:g" < "${COM_ATMOS_TRACK}/avno.t${cyc}z.cyclone.trackatcfunix" \
        > "${ARCDIR}/atcfunix.${RUN}.${PDY}${cyc}"
    sed "s:AVNO:${PSLOT4}:g" < "${COM_ATMOS_TRACK}/avnop.t${cyc}z.cyclone.trackatcfunix" \
        > "${ARCDIR}/atcfunixp.${RUN}.${PDY}${cyc}"
fi

if [[ "${RUN}" == "gdas" ]] && [[ -s "${COM_ATMOS_TRACK}/gdas.t${cyc}z.cyclone.trackatcfunix" ]]; then
    # shellcheck disable=2153
    PSLOT4=${PSLOT:0:4}
    # shellcheck disable=
    PSLOT4=${PSLOT4^^}
    sed "s:AVNO:${PSLOT4}:g" < "${COM_ATMOS_TRACK}/gdas.t${cyc}z.cyclone.trackatcfunix" \
        > "${ARCDIR}/atcfunix.${RUN}.${PDY}${cyc}"
    sed "s:AVNO:${PSLOT4}:g" < "${COM_ATMOS_TRACK}/gdasp.t${cyc}z.cyclone.trackatcfunix" \
        > "${ARCDIR}/atcfunixp.${RUN}.${PDY}${cyc}"
fi

if [ "${RUN}" = "gfs" ]; then
    nb_copy "${COM_ATMOS_GENESIS}/storms.gfso.atcf_gen.${PDY}${cyc}"      "${ARCDIR}/."
    nb_copy "${COM_ATMOS_GENESIS}/storms.gfso.atcf_gen.altg.${PDY}${cyc}" "${ARCDIR}/."
    nb_copy "${COM_ATMOS_TRACK}/trak.gfso.atcfunix.${PDY}${cyc}"          "${ARCDIR}/."
    nb_copy "${COM_ATMOS_TRACK}/trak.gfso.atcfunix.altg.${PDY}${cyc}"     "${ARCDIR}/."

    mkdir -p "${ARCDIR}/tracker.${PDY}${cyc}/${RUN}"
    blist="epac natl"
    for basin in ${blist}; do
        if [[ -f ${basin} ]]; then
            cp -rp "${COM_ATMOS_TRACK}/${basin}" "${ARCDIR}/tracker.${PDY}${cyc}/${RUN}"
        fi
    done
fi

# Archive required gaussian gfs forecast files for Fit2Obs
if [[ "${RUN}" == "gfs" ]] && [[ "${FITSARC}" = "YES" ]]; then
    VFYARC=${VFYARC:-${ROTDIR}/vrfyarch}
    [[ ! -d ${VFYARC} ]] && mkdir -p "${VFYARC}"
    mkdir -p "${VFYARC}/${RUN}.${PDY}/${cyc}"
    prefix="${RUN}.t${cyc}z"
    fhmax=${FHMAX_FITS:-${FHMAX_GFS}}
    fhr=0
    while [[ ${fhr} -le ${fhmax} ]]; do
        fhr3=$(printf %03i "${fhr}")
        sfcfile="${COM_ATMOS_MASTER}/${prefix}.sfcf${fhr3}.nc"
        sigfile="${COM_ATMOS_MASTER}/${prefix}.atmf${fhr3}.nc"
        nb_copy "${sfcfile}" "${VFYARC}/${RUN}.${PDY}/${cyc}/"
        nb_copy "${sigfile}" "${VFYARC}/${RUN}.${PDY}/${cyc}/"
        (( fhr = 10#${fhr} + 6 ))
    done
fi


###############################################################
# Archive data either to HPSS or locally
if [[ ${HPSSARCH} = "YES" || ${LOCALARCH} = "YES" ]]; then
###############################################################

    # --set the archiving command and create local directories, if necessary
    TARCMD="htar"
    if [[ ${LOCALARCH} = "YES" ]]; then
       TARCMD="tar"
       [[ ! -d "${ATARDIR}/${PDY}${cyc}" ]] && mkdir -p "${ATARDIR}/${PDY}${cyc}"
       [[ ! -d "${ATARDIR}/${CDATE_MOS}" ]] && [[ -d "${ROTDIR}/gfsmos.${PDY_MOS}" ]] && [[ "${cyc}" -eq 18 ]] && mkdir -p "${ATARDIR}/${CDATE_MOS}"
    fi

    #--determine when to save ICs for warm start and forecast-only runs
    SAVEWARMICA="NO"
    SAVEWARMICB="NO"
    SAVEFCSTIC="NO"
    firstday=$(${NDATE} +24 "${SDATE}")
    mm="${PDY:2:2}"
    dd="${PDY:4:2}"
    # TODO: This math yields multiple dates sharing the same nday
    nday=$(( (10#${mm}-1)*30+10#${dd} ))
    mod=$((nday % ARCH_WARMICFREQ))
    if [[ "${PDY}${cyc}" -eq "${firstday}" ]] && [[ "${cyc}" -eq "${ARCHINC_CYC}" ]]; then SAVEWARMICA="YES" ; fi
    if [[ "${PDY}${cyc}" -eq "${firstday}" ]] && [[ "${cyc}" -eq "${ARCHICS_CYC}" ]]; then SAVEWARMICB="YES" ; fi
    if [[ "${mod}" -eq 0 ]] && [[ "${cyc}" -eq "${ARCHINC_CYC}" ]]; then SAVEWARMICA="YES" ; fi
    if [[ "${mod}" -eq 0 ]] && [[ "${cyc}" -eq "${ARCHICS_CYC}" ]]; then SAVEWARMICB="YES" ; fi

    if [[ "${ARCHICS_CYC}" -eq 18 ]]; then
        nday1=$((nday+1))
        mod1=$((nday1 % ARCH_WARMICFREQ))
        if [[ "${mod1}" -eq 0 ]] && [[ "${cyc}" -eq "${ARCHICS_CYC}" ]] ; then SAVEWARMICB="YES" ; fi
        if [[ "${mod1}" -ne 0 ]] && [[ "${cyc}" -eq "${ARCHICS_CYC}" ]] ; then SAVEWARMICB="NO" ; fi
        if [[ "${PDY}${cyc}" -eq "${SDATE}" ]] && [[ "${cyc}" -eq "${ARCHICS_CYC}" ]] ; then SAVEWARMICB="YES" ; fi
    fi

    mod=$((nday % ARCH_FCSTICFREQ))
    if [[ "${mod}" -eq 0 ]] || [[ "${PDY}${cyc}" -eq "${firstday}" ]]; then SAVEFCSTIC="YES" ; fi


    ARCH_LIST="${DATA}/archlist"
    [[ -d ${ARCH_LIST} ]] && rm -rf "${ARCH_LIST}"
    mkdir -p "${ARCH_LIST}"
    cd "${ARCH_LIST}" || exit 2

    "${HOMEgfs}/ush/hpssarch_gen.sh" "${RUN}"
    status=$?
    if [ "${status}" -ne 0  ]; then
        echo "${HOMEgfs}/ush/hpssarch_gen.sh ${RUN} failed, ABORT!"
        exit "${status}"
    fi

    cd "${ROTDIR}" || exit 2

    if [[ "${RUN}" = "gfs" ]]; then

        targrp_list="gfsa gfsb"

        if [ "${ARCH_GAUSSIAN:-"NO"}" = "YES" ]; then
            targrp_list="${targrp_list} gfs_flux gfs_netcdfb gfs_pgrb2b"
            if [ "${MODE}" = "cycled" ]; then
              targrp_list="${targrp_list} gfs_netcdfa"
            fi
        fi

        if [ "${DO_WAVE}" = "YES" ] && [ "${WAVE_RUN}" != "gdas" ]; then
            targrp_list="${targrp_list} gfswave"
        fi

        if [ "${DO_OCN}" = "YES" ]; then
            targrp_list="${targrp_list} ocn_ice_grib2_0p5 ocn_ice_grib2_0p25 ocn_2D ocn_3D ocn_xsect ocn_daily gfs_flux_1p00"
        fi

        if [ "${DO_ICE}" = "YES" ]; then
            targrp_list="${targrp_list} ice"
        fi

        # Aerosols
        if [ "${DO_AERO}" = "YES" ]; then
            for targrp in chem; do
                # TODO: Why is this tar being done here instead of being added to the list?
                ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${targrp}.tar" $(cat "${ARCH_LIST}/${targrp}.txt")
                status=$?
                if [[ "${status}" -ne 0 ]] && [[ "${PDY}${cyc}" -ge "${firstday}" ]]; then
                    echo "HTAR ${PDY}${cyc} ${targrp}.tar failed"
                    exit "${status}"
                fi
            done
        fi

        #for restarts
        if [ "${SAVEFCSTIC}" = "YES" ]; then
            targrp_list="${targrp_list} gfs_restarta"
        fi

        #for downstream products
        if [ "${DO_BUFRSND}" = "YES" ] || [ "${WAFSF}" = "YES" ]; then
            targrp_list="${targrp_list} gfs_downstream"
        fi

        #--save mdl gfsmos output from all cycles in the 18Z archive directory
        if [[ -d "gfsmos.${PDY_MOS}" ]] && [[ "${cyc}" -eq 18 ]]; then
            set +e
            # TODO: Why is this tar being done here instead of being added to the list?
            ${TARCMD} -P -cvf "${ATARDIR}/${CDATE_MOS}/gfsmos.tar" "./gfsmos.${PDY_MOS}"
            status=$?
            if [[ "${status}" -ne 0 ]] && [[ "${PDY}${cyc}" -ge "${firstday}" ]]; then
                echo "${TARCMD^^} ${PDY}${cyc} gfsmos.tar failed"
                exit "${status}"
            fi
            set_strict
        fi
    elif [[ "${RUN}" = "gdas" ]]; then

        targrp_list="gdas"

        #gdaswave
        if [ "${DO_WAVE}" = "YES" ]; then
            targrp_list="${targrp_list} gdaswave"
        fi

        #gdasocean
        if [ "${DO_OCN}" = "YES" ]; then
            targrp_list="${targrp_list} gdasocean"
        fi

        #gdasice
        if [ "${DO_ICE}" = "YES" ]; then
            targrp_list="${targrp_list} gdasice"
        fi

        if [ "${SAVEWARMICA}" = "YES" ] || [ "${SAVEFCSTIC}" = "YES" ]; then
            targrp_list="${targrp_list} gdas_restarta"
            if [ "${DO_WAVE}" = "YES" ]; then targrp_list="${targrp_list} gdaswave_restart"; fi
            if [ "${DO_OCN}" = "YES" ]; then targrp_list="${targrp_list} gdasocean_restart"; fi
            if [ "${DO_ICE}" = "YES" ]; then targrp_list="${targrp_list} gdasice_restart"; fi
        fi

        if [ "${SAVEWARMICB}" = "YES" ] || [ "${SAVEFCSTIC}" = "YES" ]; then
            targrp_list="${targrp_list} gdas_restartb"
        fi
    fi

    # Turn on extended globbing options
    shopt -s extglob
    for targrp in ${targrp_list}; do
        set +e
        ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${targrp}.tar" $(cat "${ARCH_LIST}/${targrp}.txt")
        status=$?
        if [ "${status}" -ne 0 ] && [ "${PDY}${cyc}" -ge "${firstday}" ]; then
            echo "${TARCMD^^} ${PDY}${cyc} ${targrp}.tar failed"
            exit "${status}"
        fi
        set_strict
    done
    # Turn extended globbing back off
    shopt -u extglob

###############################################################
fi  ##end of HPSS archive
###############################################################



###############################################################
# Clean up previous cycles; various depths
# PRIOR CYCLE: Leave the prior cycle alone
GDATE=$(${NDATE} -"${assim_freq}" "${PDY}${cyc}")

# PREVIOUS to the PRIOR CYCLE
GDATE=$(${NDATE} -"${assim_freq}" "${GDATE}")
gPDY="${GDATE:0:8}"
gcyc="${GDATE:8:2}"

# Remove the TMPDIR directory
# TODO Only prepbufr is currently using this directory, and all jobs should be
#   cleaning up after themselves anyway
COMIN="${DATAROOT}/${GDATE}"
[[ -d ${COMIN} ]] && rm -rf "${COMIN}"

if [[ "${DELETE_COM_IN_ARCHIVE_JOB:-YES}" == NO ]] ; then
    exit 0
fi

# Step back every assim_freq hours and remove old rotating directories
# for successful cycles (defaults from 24h to 120h).  If GLDAS is
# active, retain files needed by GLDAS update.  Independent of GLDAS,
# retain files needed by Fit2Obs
# TODO: This whole section needs to be revamped to remove marine component
#  directories and not look at the rocoto log.
DO_GLDAS=${DO_GLDAS:-"NO"}
GDATEEND=$(${NDATE} -"${RMOLDEND:-24}"  "${PDY}${cyc}")
GDATE=$(${NDATE} -"${RMOLDSTD:-120}" "${PDY}${cyc}")
GLDAS_DATE=$(${NDATE} -96 "${PDY}${cyc}")
RTOFS_DATE=$(${NDATE} -48 "${PDY}${cyc}")
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
    if (( ${#file_list[@]} == 0 )); then return; fi
    # echo "Number of files to remove before exclusions: ${#file_list[@]}"
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

while [ "${GDATE}" -le "${GDATEEND}" ]; do
    gPDY="${GDATE:0:8}"
    gcyc="${GDATE:8:2}"
    COMINrtofs="${ROTDIR}/rtofs.${gPDY}"
    if [ -d "${COM_TOP}" ]; then
        rocotolog="${EXPDIR}/logs/${GDATE}.log"
        if [ -f "${rocotolog}" ]; then
            set +e
            testend=$(tail -n 1 "${rocotolog}" | grep "This cycle is complete: Success")
            rc=$?
            set_strict

            if [ "${rc}" -eq 0 ]; then
                # Obs
                exclude_list="prepbufr"
                templates="COM_OBS"
                for template in ${templates}; do
                    YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                    remove_files "${directory}" "${exclude_list[@]}"
                done

                # Atmos
                exclude_list="cnvstat atmanl.nc"
                if [[ ${DO_GLDAS} == "YES" ]] && [[ ${RUN} =~ "gdas" ]] && [[ "${GDATE}" -ge "${GLDAS_DATE}" ]]; then
                    exclude_list="${exclude_list} sflux sfcanl"
                fi
                templates=$(compgen -A variable | grep 'COM_ATMOS_.*_TMPL')
                for template in ${templates}; do
                    YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                    remove_files "${directory}" "${exclude_list[@]}"
                done

                # Wave
                exclude_list=""
                templates=$(compgen -A variable | grep 'COM_WAVE_.*_TMPL')
                for template in ${templates}; do
                    YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                    remove_files "${directory}" "${exclude_list[@]}"
                done

                # Ocean
                exclude_list=""
                templates=$(compgen -A variable | grep 'COM_OCEAN_.*_TMPL')
                for template in ${templates}; do
                    YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                    remove_files "${directory}" "${exclude_list[@]}"
                done

                # Ice
                exclude_list=""
                templates=$(compgen -A variable | grep 'COM_ICE_.*_TMPL')
                for template in ${templates}; do
                    YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                    remove_files "${directory}" "${exclude_list[@]}"
                done

                # Aerosols (GOCART)
                exclude_list=""
                templates=$(compgen -A variable | grep 'COM_CHEM_.*_TMPL')
                for template in ${templates}; do
                    YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                    remove_files "${directory}" "${exclude_list[@]}"
                done

                # Mediator
                exclude_list=""
                templates=$(compgen -A variable | grep 'COM_MED_.*_TMPL')
                for template in ${templates}; do
                    YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                    remove_files "${directory}" "${exclude_list[@]}"
                done

                if [ -d "${COMINrtofs}" ] && [ "${GDATE}" -lt "${RTOFS_DATE}" ]; then rm -rf "${COMINrtofs}" ; fi
            fi
        fi
    fi

    # Remove mdl gfsmos directory
    if [ "${RUN}" = "gfs" ]; then
        COMIN="${ROTDIR}/gfsmos.${gPDY}"
        if [ -d "${COMIN}" ] && [ "${GDATE}" -lt "${CDATE_MOS}" ]; then rm -rf "${COMIN}" ; fi
    fi

    # Remove any empty directories
    target_dir="${ROTDIR:?}/${RUN}.${gPDY}/${gcyc}/"
    if [[ -d ${target_dir} ]]; then
        find "${target_dir}" -empty -type d -delete
    fi

    GDATE=$(${NDATE} +"${assim_freq}" "${GDATE}")
done

# Remove archived gaussian files used for Fit2Obs in $VFYARC that are
# $FHMAX_FITS plus a delta before $CDATE.  Touch existing archived
# gaussian files to prevent the files from being removed by automatic
# scrubber present on some machines.

if [ "${RUN}" = "gfs" ]; then
    fhmax=$((FHMAX_FITS+36))
    RDATE=$(${NDATE} -"${fhmax}" "${PDY}${cyc}")
    rPDY=$(echo "${RDATE}" | cut -c1-8)
    COMIN="${VFYARC}/${RUN}.${rPDY}"
    [[ -d ${COMIN} ]] && rm -rf "${COMIN}"

    TDATE=$(${NDATE} -"${FHMAX_FITS}" "${PDY}${cyc}")
    while [ "${TDATE}" -lt "${PDY}${cyc}" ]; do
        tPDY=$(echo "${TDATE}" | cut -c1-8)
        tcyc=$(echo "${TDATE}" | cut -c9-10)
        TDIR=${VFYARC}/${RUN}.${tPDY}/${tcyc}
        [[ -d ${TDIR} ]] && touch "${TDIR}"/*
        TDATE=$(${NDATE} +6 "${TDATE}")
    done
fi

# Remove $RUN.$rPDY for the older of GDATE or RDATE
GDATE=$(${NDATE} -"${RMOLDSTD:-120}" "${PDY}${cyc}")
fhmax=${FHMAX_GFS}
RDATE=$(${NDATE} -"${fhmax}" "${PDY}${cyc}")
if [ "${GDATE}" -lt "${RDATE}" ]; then
    RDATE=${GDATE}
fi
rPDY=$(echo "${RDATE}" | cut -c1-8)
COMIN="${ROTDIR}/${RUN}.${rPDY}"
[[ -d ${COMIN} ]] && rm -rf "${COMIN}"


###############################################################


exit 0
