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
APREFIX="${CDUMP}.t${cyc}z."

# Realtime parallels run GFS MOS on 1 day delay
# If realtime parallel, back up CDATE_MOS one day
CDATE_MOS=${CDATE}
if [ "${REALTIME}" = "YES" ]; then
    CDATE_MOS=$(${NDATE} -24 "${CDATE}")
fi
PDY_MOS=$(echo "${CDATE_MOS}" | cut -c1-8)

###############################################################
# Archive online for verification and diagnostics
###############################################################
COMIN=${COMINatmos:-"${ROTDIR}/${CDUMP}.${PDY}/${cyc}/atmos"}
cd "${COMIN}"

source "${HOMEgfs}/ush/file_utils.sh"

[[ ! -d ${ARCDIR} ]] && mkdir -p "${ARCDIR}"
nb_copy "${APREFIX}"gsistat "${ARCDIR}"/gsistat."${CDUMP}"."${CDATE}"
nb_copy "${APREFIX}"pgrb2.1p00.anl "${ARCDIR}"/pgbanl."${CDUMP}"."${CDATE}".grib2

# Archive 1 degree forecast GRIB2 files for verification
if [ "${CDUMP}" = "gfs" ]; then
    fhmax=${FHMAX_GFS}
    fhr=0
    while [ "${fhr}" -le "${fhmax}" ]; do
        fhr2=$(printf %02i "${fhr}")
        fhr3=$(printf %03i "${fhr}")
        nb_copy "${APREFIX}"pgrb2.1p00.f"${fhr3}" "${ARCDIR}"/pgbf"${fhr2}"."${CDUMP}"."${CDATE}".grib2
        fhr=$((10#${fhr} + 10#${FHOUT_GFS} ))
    done
fi
if [ "${CDUMP}" = "gdas" ]; then
    flist="000 003 006 009"
    for fhr in ${flist}; do
        fname=${APREFIX}pgrb2.1p00.f${fhr}
        fhr2=$(printf %02i $((10#${fhr})))
        nb_copy "${fname}" "${ARCDIR}"/pgbf"${fhr2}"."${CDUMP}"."${CDATE}".grib2
    done
fi

if [ -s avno.t"${cyc}"z.cyclone.trackatcfunix ]; then
    PLSOT4=$(echo "${PSLOT}"|cut -c 1-4 |tr '[a-z]' '[A-Z]')
    cat avno.t"${cyc}"z.cyclone.trackatcfunix | sed s:AVNO:"${PLSOT4}":g  > "${ARCDIR}"/atcfunix."${CDUMP}"."${CDATE}"
    cat avnop.t"${cyc}"z.cyclone.trackatcfunix | sed s:AVNO:"${PLSOT4}":g  > "${ARCDIR}"/atcfunixp."${CDUMP}"."${CDATE}"
fi

if [ "${CDUMP}" = "gdas" ] && [ -s gdas.t"${cyc}"z.cyclone.trackatcfunix ]; then
    PLSOT4=$(echo "${PSLOT}"|cut -c 1-4 |tr '[a-z]' '[A-Z]')
    cat gdas.t"${cyc}"z.cyclone.trackatcfunix | sed s:AVNO:"${PLSOT4}":g  > "${ARCDIR}"/atcfunix."${CDUMP}"."${CDATE}"
    cat gdasp.t"${cyc}"z.cyclone.trackatcfunix | sed s:AVNO:"${PLSOT4}":g  > "${ARCDIR}"/atcfunixp."${CDUMP}"."${CDATE}"
fi

if [ "${CDUMP}" = "gfs" ]; then
    nb_copy storms.gfso.atcf_gen."${CDATE}"      "${ARCDIR}"/.
    nb_copy storms.gfso.atcf_gen.altg."${CDATE}" "${ARCDIR}"/.
    nb_copy trak.gfso.atcfunix."${CDATE}"        "${ARCDIR}"/.
    nb_copy trak.gfso.atcfunix.altg."${CDATE}"   "${ARCDIR}"/.

    mkdir -p "${ARCDIR}"/tracker."${CDATE}"/"${CDUMP}"
    blist="epac natl"
    for basin in ${blist}; do
        if [[ -f ${basin} ]]; then
               cp -rp "${basin}" "${ARCDIR}"/tracker."${CDATE}"/"${CDUMP}"
        fi
    done
fi

# Archive required gaussian gfs forecast files for Fit2Obs
if [ "${CDUMP}" = "gfs" ] && [ "${FITSARC}" = "YES" ]; then
    VFYARC=${VFYARC:-${ROTDIR}/vrfyarch}
    [[ ! -d ${VFYARC} ]] && mkdir -p "${VFYARC}"
    mkdir -p "${VFYARC}"/"${CDUMP}"."${PDY}"/"${cyc}"
    prefix=${CDUMP}.t${cyc}z
    fhmax=${FHMAX_FITS:-${FHMAX_GFS}}
    fhr=0
    while [[ ${fhr} -le ${fhmax} ]]; do
        fhr3=$(printf %03i "${fhr}")
        sfcfile=${prefix}.sfcf${fhr3}.nc
        sigfile=${prefix}.atmf${fhr3}.nc
        nb_copy "${sfcfile}" "${VFYARC}"/"${CDUMP}"."${PDY}"/"${cyc}"/
        nb_copy "${sigfile}" "${VFYARC}"/"${CDUMP}"."${PDY}"/"${cyc}"/
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
   [ ! -d "${ATARDIR}"/"${CDATE}" ] && mkdir -p "${ATARDIR}"/"${CDATE}"
   [ ! -d "${ATARDIR}"/"${CDATE_MOS}" ] && [ -d "${ROTDIR}"/gfsmos."${PDY_MOS}" ] && [ "${cyc}" -eq 18 ] && mkdir -p "${ATARDIR}"/"${CDATE_MOS}"
fi

#--determine when to save ICs for warm start and forecast-only runs
SAVEWARMICA="NO"
SAVEWARMICB="NO"
SAVEFCSTIC="NO"
firstday=$(${NDATE} +24 "${SDATE}")
mm=$(echo "${CDATE}"|cut -c 5-6)
dd=$(echo "${CDATE}"|cut -c 7-8)
nday=$(( (10#${mm}-1)*30+10#${dd} ))
mod=$((nday % ARCH_WARMICFREQ))
if [ "${CDATE}" -eq "${firstday}" ] && [ "${cyc}" -eq "${ARCHINC_CYC}" ]; then SAVEWARMICA="YES" ; fi
if [ "${CDATE}" -eq "${firstday}" ] && [ "${cyc}" -eq "${ARCHICS_CYC}" ]; then SAVEWARMICB="YES" ; fi
if [ "${mod}" -eq 0 ] && [ "${cyc}" -eq "${ARCHINC_CYC}" ]; then SAVEWARMICA="YES" ; fi
if [ "${mod}" -eq 0 ] && [ "${cyc}" -eq "${ARCHICS_CYC}" ]; then SAVEWARMICB="YES" ; fi

if [ "${ARCHICS_CYC}" -eq 18 ]; then
    nday1=$((nday+1))
    mod1=$((nday1 % ARCH_WARMICFREQ))
    if [ "${mod1}" -eq 0 ] && [ "${cyc}" -eq "${ARCHICS_CYC}" ] ; then SAVEWARMICB="YES" ; fi
    if [ "${mod1}" -ne 0 ] && [ "${cyc}" -eq "${ARCHICS_CYC}" ] ; then SAVEWARMICB="NO" ; fi
    if [ "${CDATE}" -eq "${SDATE}" ] && [ "${cyc}" -eq "${ARCHICS_CYC}" ] ; then SAVEWARMICB="YES" ; fi
fi

mod=$((nday % ARCH_FCSTICFREQ))
if [ "${mod}" -eq 0 ] || [ "${CDATE}" -eq "${firstday}" ]; then SAVEFCSTIC="YES" ; fi


ARCH_LIST="${COMIN}/archlist"
[[ -d ${ARCH_LIST} ]] && rm -rf "${ARCH_LIST}"
mkdir -p "${ARCH_LIST}"
cd "${ARCH_LIST}"

"${HOMEgfs}"/ush/hpssarch_gen.sh "${CDUMP}"
status=$?
if [ "${status}" -ne 0  ]; then
    echo "${HOMEgfs}/ush/hpssarch_gen.sh ${CDUMP} failed, ABORT!"
    exit "${status}"
fi

cd "${ROTDIR}"

if [ "${CDUMP}" = "gfs" ]; then

    targrp_list="gfsa gfsb"

    if [ "${ARCH_GAUSSIAN:-"NO"}" = "YES" ]; then
        targrp_list="${targrp_list} gfs_flux gfs_netcdfb gfs_pgrb2b"
        if [ "${MODE}" = "cycled" ]; then
          targrp_list="${targrp_list} gfs_netcdfa"
        fi
    fi

    if [ "${DO_WAVE}" = "YES" ] && [ "${WAVE_CDUMP}" != "gdas" ]; then
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
            ${TARCMD} -P -cvf "${ATARDIR}"/"${CDATE}"/"${targrp}".tar $(cat "${ARCH_LIST}"/"${targrp}".txt)
            status=$?
            if [ "${status}" -ne 0 ] && [ "${CDATE}" -ge "${firstday}" ]; then
                echo "HTAR ${CDATE} ${targrp}.tar failed"
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
    if [ -d gfsmos."${PDY_MOS}" ] && [ "${cyc}" -eq 18 ]; then
        set +e
        ${TARCMD} -P -cvf "${ATARDIR}"/"${CDATE_MOS}"/gfsmos.tar ./gfsmos."${PDY_MOS}"
        status=$?
        if [ "${status}" -ne 0 ] && [ "${CDATE}" -ge "${firstday}" ]; then
            echo "$(echo "${TARCMD}" | tr 'a-z' 'A-Z') ${CDATE} gfsmos.tar failed"
            exit "${status}"
        fi
        set_strict
    fi
elif [ "${CDUMP}" = "gdas" ]; then

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

        if [ "${DO_WAVE}" = "YES" ]; then
            targrp_list="${targrp_list} gdaswave_restart"
        fi
        if [ "${DO_OCN}" = "YES" ]; then
            targrp_list="${targrp_list} gdasocean_restart"
        fi
        if [ "${DO_ICE}" = "YES" ]; then
            targrp_list="${targrp_list} gdasice_restart"
        fi
    fi

    if [ "${SAVEWARMICB}" = "YES" ] || [ "${SAVEFCSTIC}" = "YES" ]; then
        targrp_list="${targrp_list} gdas_restartb"
    fi
fi

# Turn on extended globbing options
shopt -s extglob
for targrp in ${targrp_list}; do
    set +e
    ${TARCMD} -P -cvf "${ATARDIR}"/"${CDATE}"/"${targrp}".tar $(cat "${ARCH_LIST}"/"${targrp}".txt)
    status=$?
    if [ "${status}" -ne 0 ] && [ "${CDATE}" -ge "${firstday}" ]; then
        echo "$(echo "${TARCMD}" | tr 'a-z' 'A-Z') ${CDATE} ${targrp}.tar failed"
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
GDATE=$(${NDATE} -"${assim_freq}" "${CDATE}")

# PREVIOUS to the PRIOR CYCLE
GDATE=$(${NDATE} -"${assim_freq}" "${GDATE}")
gPDY=$(echo "${GDATE}" | cut -c1-8)
gcyc=$(echo "${GDATE}" | cut -c9-10)

# Remove the TMPDIR directory
COMIN="${RUNDIR}/${GDATE}"
[[ -d ${COMIN} ]] && rm -rf "${COMIN}"

if [[ "${DELETE_COM_IN_ARCHIVE_JOB:-YES}" == NO ]] ; then
    exit 0
fi

# Step back every assim_freq hours and remove old rotating directories
# for successful cycles (defaults from 24h to 120h).  If GLDAS is
# active, retain files needed by GLDAS update.  Independent of GLDAS,
# retain files needed by Fit2Obs
DO_GLDAS=${DO_GLDAS:-"NO"}
GDATEEND=$(${NDATE} -"${RMOLDEND:-24}"  "${CDATE}")
GDATE=$(${NDATE} -"${RMOLDSTD:-120}" "${CDATE}")
GLDAS_DATE=$(${NDATE} -96 "${CDATE}")
RTOFS_DATE=$(${NDATE} -48 "${CDATE}")
while [ "${GDATE}" -le "${GDATEEND}" ]; do
    gPDY=$(echo "${GDATE}" | cut -c1-8)
    gcyc=$(echo "${GDATE}" | cut -c9-10)
    COMIN="${ROTDIR}/${CDUMP}.${gPDY}/${gcyc}/atmos"
    COMINwave="${ROTDIR}/${CDUMP}.${gPDY}/${gcyc}/wave"
    COMINocean="${ROTDIR}/${CDUMP}.${gPDY}/${gcyc}/ocean"
    COMINice="${ROTDIR}/${CDUMP}.${gPDY}/${gcyc}/ice"
    COMINmed="${ROTDIR}/${CDUMP}.${gPDY}/${gcyc}/med"
    COMINrtofs="${ROTDIR}/rtofs.${gPDY}"
    if [ -d "${COMIN}" ]; then
        rocotolog="${EXPDIR}/logs/${GDATE}.log"
        if [ -f "${rocotolog}" ]; then
            set +e
            testend=$(tail -n 1 "${rocotolog}" | grep "This cycle is complete: Success")
            rc=$?
            set_strict
            if [ "${rc}" -eq 0 ]; then
                if [ -d "${COMINwave}" ]; then rm -rf "${COMINwave}" ; fi
                if [ -d "${COMINocean}" ]; then rm -rf "${COMINocean}" ; fi
                if [ -d "${COMINice}" ]; then rm -rf "${COMINice}" ; fi
                if [ -d "${COMINmed}" ]; then rm -rf "${COMINmed}" ; fi
                if [ -d "${COMINrtofs}" ] && [ "${GDATE}" -lt "${RTOFS_DATE}" ]; then rm -rf "${COMINrtofs}" ; fi
                if [ "${CDUMP}" != "gdas" ] || [ "${DO_GLDAS}" = "NO" ] || [ "${GDATE}" -lt "${GLDAS_DATE}" ]; then
                    if [ "${CDUMP}" = "gdas" ]; then
                        for file in $(ls "${COMIN}" |grep -v prepbufr |grep -v cnvstat |grep -v atmanl.nc); do
                            rm -rf "${COMIN}"/"${file}"
                        done
                    else
                        rm -rf "${COMIN}"
                    fi
                else
                    if [ "${DO_GLDAS}" = "YES" ]; then
                        for file in $(ls "${COMIN}" |grep -v sflux |grep -v RESTART |grep -v prepbufr |grep -v cnvstat |grep -v atmanl.nc); do
                            rm -rf "${COMIN}"/"${file}"
                        done
                        for file in $(ls "${COMIN}"/RESTART |grep -v sfcanl ); do
                            rm -rf "${COMIN}"/RESTART/"${file}"
                        done
                    else
                        for file in $(ls "${COMIN}" |grep -v prepbufr |grep -v cnvstat |grep -v atmanl.nc); do
                            rm -rf "${COMIN}"/"${file}"
                        done
                    fi
                fi
            fi
        fi
    fi

    # Remove any empty directories
    if [ -d "${COMIN}" ]; then
        [[ ! "$(ls -A "${COMIN}")" ]] && rm -rf "${COMIN}"
    fi

    if [ -d "${COMINwave}" ]; then
        [[ ! "$(ls -A "${COMINwave}")" ]] && rm -rf "${COMINwave}"
    fi

    if [ -d "${COMINocean}" ]; then
        [[ ! "$(ls -A "${COMINocean}")" ]] && rm -rf "${COMINocean}"
    fi

    if [ -d "${COMINice}" ]; then
        [[ ! "$(ls -A "${COMINice}")" ]] && rm -rf "${COMINice}"
    fi

    if [ -d "${COMINmed}" ]; then
        [[ ! "$(ls -A "${COMINmed}")" ]] && rm -rf "${COMINmed}"
    fi

    # Remove mdl gfsmos directory
    if [ "${CDUMP}" = "gfs" ]; then
        COMIN="${ROTDIR}/gfsmos.${gPDY}"
        if [ -d "${COMIN}" ] && [ "${GDATE}" -lt "${CDATE_MOS}" ]; then rm -rf "${COMIN}" ; fi
    fi

    GDATE=$(${NDATE} +"${assim_freq}" "${GDATE}")
done

# Remove archived gaussian files used for Fit2Obs in $VFYARC that are
# $FHMAX_FITS plus a delta before $CDATE.  Touch existing archived
# gaussian files to prevent the files from being removed by automatic
# scrubber present on some machines.

if [ "${CDUMP}" = "gfs" ]; then
    fhmax=$((FHMAX_FITS+36))
    RDATE=$(${NDATE} -"${fhmax}" "${CDATE}")
    rPDY=$(echo "${RDATE}" | cut -c1-8)
    COMIN="${VFYARC}/${CDUMP}.${rPDY}"
    [[ -d ${COMIN} ]] && rm -rf "${COMIN}"

    TDATE=$(${NDATE} -"${FHMAX_FITS}" "${CDATE}")
    while [ "${TDATE}" -lt "${CDATE}" ]; do
        tPDY=$(echo "${TDATE}" | cut -c1-8)
        tcyc=$(echo "${TDATE}" | cut -c9-10)
        TDIR=${VFYARC}/${CDUMP}.${tPDY}/${tcyc}
        [[ -d ${TDIR} ]] && touch "${TDIR}"/*
        TDATE=$(${NDATE} +6 "${TDATE}")
    done
fi

# Remove $CDUMP.$rPDY for the older of GDATE or RDATE
GDATE=$(${NDATE} -"${RMOLDSTD:-120}" "${CDATE}")
fhmax=${FHMAX_GFS}
RDATE=$(${NDATE} -"${fhmax}" "${CDATE}")
if [ "${GDATE}" -lt "${RDATE}" ]; then
    RDATE=${GDATE}
fi
rPDY=$(echo "${RDATE}" | cut -c1-8)
COMIN="${ROTDIR}/${CDUMP}.${rPDY}"
[[ -d ${COMIN} ]] && rm -rf "${COMIN}"


###############################################################


exit 0
