#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

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
# Ignore possible spelling error (nothing is misspelled)
# shellcheck disable=SC2153
CDATE_MOS=${PDY}${cyc}
if [ "${REALTIME}" = "YES" ]; then
    CDATE_MOS=$(${NDATE} -24 "${PDY}${cyc}")
fi
PDY_MOS="${CDATE_MOS:0:8}"

###############################################################
# Archive online for verification and diagnostics
###############################################################
source "${USHgfs}/file_utils.sh"

[[ ! -d ${ARCDIR} ]] && mkdir -p "${ARCDIR}"
nb_copy "${COM_ATMOS_ANALYSIS}/${APREFIX}gsistat" "${ARCDIR}/gsistat.${RUN}.${PDY}${cyc}"
if [[ ${DO_AERO} = "YES" ]]; then
   nb_copy "${COM_CHEM_ANALYSIS}/${APREFIX}aerostat" "${ARCDIR}/aerostat.${RUN}.${PDY}${cyc}"
fi
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
        sfcfile="${COM_ATMOS_HISTORY}/${prefix}.sfcf${fhr3}.nc"
        sigfile="${COM_ATMOS_HISTORY}/${prefix}.atmf${fhr3}.nc"
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
    HSICMD="hsi"
    if [[ ${LOCALARCH} = "YES" ]]; then
       TARCMD="tar"
       HSICMD=''
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

    cd "${DATA}" || exit 2

    "${USHgfs}/hpssarch_gen.sh" "${RUN}"
    status=$?
    if [ "${status}" -ne 0  ]; then
        echo "${USHgfs}/hpssarch_gen.sh ${RUN} failed, ABORT!"
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

        if [ "${DO_WAVE}" = "YES" ]; then
            targrp_list="${targrp_list} gfswave"
        fi

        if [[ "${DO_OCN}" == "YES" ]]; then
            targrp_list="${targrp_list} ocean_6hravg ocean_daily ocean_grib2 gfs_flux_1p00"
        fi

        if [[ "${DO_ICE}" == "YES" ]]; then
            targrp_list="${targrp_list} ice_6hravg ice_grib2"
        fi

        # Aerosols
        if [ "${DO_AERO}" = "YES" ]; then
            for targrp in chem; do
                # TODO: Why is this tar being done here instead of being added to the list?
                ${TARCMD} -P -cvf "${ATARDIR}/${PDY}${cyc}/${targrp}.tar" $(cat "${DATA}/${targrp}.txt")
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
        if [ "${DO_BUFRSND}" = "YES" ]; then
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
            targrp_list="${targrp_list} gdasocean gdasocean_analysis"
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

        # Test whether gdas.tar or gdas_restarta.tar will have rstprod data
        has_rstprod="NO"
        case ${targrp} in
            'gdas'|'gdas_restarta')
                # Test for rstprod in each archived file
                while IFS= read -r file; do
                    if [[ -f ${file} ]]; then
                        group=$( stat -c "%G" "${file}" )
                        if [[ "${group}" == "rstprod" ]]; then
                            has_rstprod="YES"
                            break
                        fi
                    fi
                done < "${DATA}/${targrp}.txt"

                ;;
            *) ;;
        esac

        # Create the tarball
        tar_fl="${ATARDIR}/${PDY}${cyc}/${targrp}.tar"
        ${TARCMD} -P -cvf "${tar_fl}" $(cat "${DATA}/${targrp}.txt")
        status=$?

        # Change group to rstprod if it was found even if htar/tar failed in case of partial creation
        if [[ "${has_rstprod}" == "YES" ]]; then
            ${HSICMD} chgrp rstprod "${tar_fl}"
            stat_chgrp=$?
            ${HSICMD} chmod 640 "${tar_fl}"
            stat_chgrp=$((stat_chgrp+$?))
            if [ "${stat_chgrp}" -gt 0 ]; then
                echo "FATAL ERROR: Unable to properly restrict ${tar_fl}!"
                echo "Attempting to delete ${tar_fl}"
                ${HSICMD} rm "${tar_fl}"
                echo "Please verify that ${tar_fl} was deleted!"
                exit "${stat_chgrp}"
            fi
        fi

        # For safety, test if the htar/tar command failed after changing groups
        if [[ "${status}" -ne 0 ]] && [[ "${PDY}${cyc}" -ge "${firstday}" ]]; then
            echo "FATAL ERROR: ${TARCMD} ${tar_fl} failed"
            exit "${status}"
        fi
        set_strict
    done
    # Turn extended globbing back off
    shopt -u extglob

###############################################################
fi  ##end of HPSS archive
###############################################################

exit 0
