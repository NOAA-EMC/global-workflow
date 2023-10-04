#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Clean up previous cycles; various depths
# PRIOR CYCLE: Leave the prior cycle alone
# shellcheck disable=SC2153
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
# for successful cycles (defaults from 24h to 120h).
# Retain files needed by Fit2Obs
# TODO: This whole section needs to be revamped to remove marine component
#  directories and not look at the rocoto log.
GDATEEND=$(${NDATE} -"${RMOLDEND:-24}"  "${PDY}${cyc}")
GDATE=$(${NDATE} -"${RMOLDSTD:-120}" "${PDY}${cyc}")
RTOFS_DATE=$(${NDATE} -48 "${PDY}${cyc}")
function remove_files() {
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
    # Ignore compound command warning
    # shellcheck disable=SC2312
    readarray -t file_list < <(find -L "${directory}" -type f)
    if (( ${#file_list[@]} == 0 )); then return; fi
    # echo "Number of files to remove before exclusions: ${#file_list[@]}"
    for exclude in ${exclude_list}; do
        echo "Excluding ${exclude}"
        declare -a file_list_old=("${file_list[@]}")
        # Ignore compound command warning
        # shellcheck disable=SC2312
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

while (( GDATE <= GDATEEND )); do
    gPDY="${GDATE:0:8}"
    gcyc="${GDATE:8:2}"
    COMINrtofs="${ROTDIR}/rtofs.${gPDY}"
    YMD="${gPDY}" HH="${gcyc}" generate_com COM_TOP
    if [[ -d "${COM_TOP}" ]]; then
        rocotolog="${EXPDIR}/logs/${GDATE}.log"
        if [[ -f "${rocotolog}" ]]; then
            # shellcheck disable=SC2312
            if [[ $(tail -n 1 "${rocotolog}") =~ "This cycle is complete: Success" ]]; then
                case ${RUN} in
                    gdas)   nmem="${NMEM_ENS}";;
                    gfs)    nmem="${NMEM_ENS_GFS}";;
                    *)
                        echo "FATAL ERROR: Unknown RUN ${RUN} during cleanup"
                        exit 10
                        ;;
                esac

                memlist=("") # Empty MEMDIR for deterministic

                RUN="enkf${RUN}" YMD="${gPDY}" HH="${gcyc}" generate_com enkf_top:COM_TOP_TMPL
                if [[ -d ${enkf_top} ]]; then
                    # Add ensemble directories if they exist
                    readarray -O"${#memlist[@]}" memlist< <(seq --format="mem%03g" 1 "${nmem}")
                    memlist+=("ensstat")
                fi

                for MEMDIR in "${memlist[@]}"; do

                    if [[ -n "${MEMDIR}" ]]; then
                        local_run="enkf${RUN}"
                    else
                        local_run="${RUN}"
                    fi

                    # Obs
                    exclude_list="prepbufr"
                    templates="COM_OBS"
                    for template in ${templates}; do
                        RUN="${local_run}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                        remove_files "${directory}" "${exclude_list[@]}"
                    done

                    # Atmos
                    exclude_list="cnvstat atmanl.nc"
                    templates=$(compgen -A variable | grep 'COM_ATMOS_.*_TMPL')
                    for template in ${templates}; do
                        RUN="${local_run}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                        remove_files "${directory}" "${exclude_list[@]}"
                    done

                    # Wave
                    exclude_list=""
                    templates=$(compgen -A variable | grep 'COM_WAVE_.*_TMPL')
                    for template in ${templates}; do
                        RUN="${local_run}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                        remove_files "${directory}" "${exclude_list[@]}"
                    done

                    # Ocean
                    exclude_list=""
                    templates=$(compgen -A variable | grep 'COM_OCEAN_.*_TMPL')
                    for template in ${templates}; do
                        RUN="${local_run}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                        remove_files "${directory}" "${exclude_list[@]}"
                    done

                    # Ice
                    exclude_list=""
                    templates=$(compgen -A variable | grep 'COM_ICE_.*_TMPL')
                    for template in ${templates}; do
                        RUN="${local_run}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                        remove_files "${directory}" "${exclude_list[@]}"
                    done

                    # Aerosols (GOCART)
                    exclude_list=""
                    templates=$(compgen -A variable | grep 'COM_CHEM_.*_TMPL')
                    for template in ${templates}; do
                        RUN="${local_run}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                        remove_files "${directory}" "${exclude_list[@]}"
                    done

                    # Mediator
                    exclude_list=""
                    templates=$(compgen -A variable | grep 'COM_MED_.*_TMPL')
                    for template in ${templates}; do
                        RUN="${local_run}" YMD="${gPDY}" HH="${gcyc}" generate_com "directory:${template}"
                        remove_files "${directory}" "${exclude_list[@]}"
                    done

                done

                if [[ -d "${COMINrtofs}" ]] && (( GDATE < RTOFS_DATE )); then rm -rf "${COMINrtofs}" ; fi
            fi
        fi
    fi

    # Remove mdl gfsmos directory
    if [[ "${RUN}" == "gfs" ]]; then
        COMIN="${ROTDIR}/gfsmos.${gPDY}"
        if [[ -d "${COMIN}" ]] && (( GDATE < CDATE_MOS )); then rm -rf "${COMIN}" ; fi
    fi

    # Remove any empty directories
    target_dir="${ROTDIR:?}/${RUN}.${gPDY}/${gcyc}/"
    if [[ -d "${target_dir}" ]]; then
        find "${target_dir}" -empty -type d -delete
    fi

    GDATE=$(${NDATE} +"${assim_freq}" "${GDATE}")
done

# Remove archived gaussian files used for Fit2Obs in $VFYARC that are
# $FHMAX_FITS plus a delta before $CDATE. Touch existing archived
# gaussian files to prevent the files from being removed by automatic
# scrubber present on some machines.

if [[ "${RUN}" == "gfs" ]]; then
    fhmax=$((FHMAX_FITS + 36))
    RDATE=$(${NDATE} -"${fhmax}" "${PDY}${cyc}")
    rPDY="${RDATE:0:8}"
    COMIN="${ROTDIR}/vrfyarch/${RUN}.${rPDY}"
    [[ -d ${COMIN} ]] && rm -rf "${COMIN}"

    TDATE=$(${NDATE} -"${FHMAX_FITS}" "${PDY}${cyc}")
    while (( TDATE < "${PDY}${cyc}" )); do
        tPDY="${TDATE:0:8}"
        tcyc="${TDATE:8:2}"
        TDIR="${ROTDIR}/vrfyarch/${RUN}.${tPDY}/${tcyc}"
        [[ -d ${TDIR} ]] && touch "${TDIR}"/*
        TDATE=$(${NDATE} +6 "${TDATE}")
    done
fi

# Remove $RUN.$rPDY for the older of GDATE or RDATE
GDATE=$(${NDATE} -"${RMOLDSTD:-120}" "${PDY}${cyc}")
RDATE=$(${NDATE} -"${FHMAX_GFS}" "${PDY}${cyc}")
if (( GDATE < RDATE )); then
    RDATE=${GDATE}
fi
rPDY="${RDATE:0:8}"
COMIN="${ROTDIR}/${RUN}.${rPDY}"
if [[ -d ${COMIN} ]]; then rm -rf "${COMIN}"; fi
