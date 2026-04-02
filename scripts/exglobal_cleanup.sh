#! /usr/bin/env bash

###############################################################
echo "Begin Cleanup ${DATAROOT}!"

# Remove DATAoutput from the forecast model run
# TODO: Handle this better
DATAfcst="${DATAROOT}/${RUN}fcst.${PDY:-}${cyc}"
if [[ -d "${DATAfcst}" ]]; then
    rm -rf "${DATAfcst}"
fi
#DATAefcs="${DATAROOT}/${RUN}efcs???${PDY:-}${cyc}"
rm -rf "${DATAROOT}/${RUN}efcs"*"${PDY:-}${cyc}"
###############################################################

if [[ "${CLEANUP_COM:-YES}" == NO ]]; then
    exit 0
fi

SELECTIVE_CLEANUP_MIN=${SELECTIVE_CLEANUP_MIN:-24}
SELECTIVE_CLEANUP_MAX=${SELECTIVE_CLEANUP_MAX:-120}
RTOFS_CLEANUP=${RTOFS_CLEANUP:-24}
GEMPAK_CLEANUP=${GEMPAK_CLEANUP:-240}
###############################################################
# Clean up previous cycles; various depths

# Step back every assim_freq hours and remove old rotating directories
# for successful cycles (defaults from 24h to 120h).
# Retain files needed by Fit2Obs
first_selective_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${SELECTIVE_CLEANUP_MAX} hours")
last_selective_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${SELECTIVE_CLEANUP_MIN} hours")
gempak_cutoff_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${GEMPAK_CLEANUP} hours")
# Selective exclude list
selective_exclude_string="${selective_exclude_string:-}"
# Gempak exclude list
gempak_exclude_string+=", gfs_1p00_*"

# RTOFS is cleaned separately (for all RUNs)
rtofs_cutoff_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${RTOFS_CLEANUP} hours")

# Find the first and last date among all cleanup targets
max_cleanup_max="${SELECTIVE_CLEANUP_MAX:-120}"
max_list="${SELECTIVE_CLEANUP_MAX}"
if [[ "${RUN}" == "gfs" && "${DO_GEMPAK}" == "YES" ]]; then
    max_list+=" ${GEMPAK_CLEANUP}"
fi

for cleanup_max in ${max_list}; do
    if [[ ${cleanup_max} -gt ${max_cleanup_max} ]]; then
        max_cleanup_max=${cleanup_max}
    fi
done

# Start 4 cycles before the earliest exclusion target so we actually remove older files
max_cleanup_max=$((max_cleanup_max + 4 * assim_freq))
first_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${max_cleanup_max} hours")

# Cleanup starts on SELECTIVE_CLEANUP_MIN
last_date=${last_selective_date}

# Declare remove_files(), which will be called in a loop to selectively remove files
function remove_files() {
    local directory=$1
    shift
    if [[ ! -d ${directory} ]]; then
        echo "No directory ${directory} to remove files from, skiping"
        return
    fi
    local find_exclude_string=""
    for exclude in "$@"; do
        if [[ -n "${exclude}" ]]; then
            find_exclude_string+=" -name ${exclude} -or"
        fi
    done
    # Chop off any trailing or
    find_exclude_string="${find_exclude_string[*]/%-or/}"
    # Remove all regular files and symlinks that do not match
    # shellcheck disable=SC2086
    if [[ -n "${find_exclude_string}" ]]; then
        # String is non-empty → use exclusion
        find "${directory}" \( -type f -o -type l \) -not \( ${find_exclude_string} \) -ignore_readdir_race -delete
    else
        # String is empty → no exclusion
        find "${directory}" \( -type f -o -type l \) -ignore_readdir_race -delete
    fi

    # Remove any empty directories
    find "${directory}" -type d -empty -delete
}

# Now start removing old COM files/directories
for ((current_date = first_date; current_date <= last_date;  \
current_date = $(date --utc +%Y%m%d%H -d "${current_date:0:8} ${current_date:8:2} +${assim_freq} hours"))); do
    current_PDY="${current_date:0:8}"
    current_cyc="${current_date:8:2}"
    rocotolog="${EXPDIR}/logs/${current_date}.log"

    # Build the exclude list based on the 'current_date'
    exclude_string=""
    if [[ ${current_date} -ge ${first_selective_date} && ${current_date} -le ${last_selective_date} ]]; then
        exclude_string+="${selective_exclude_string}"
    fi

    # Extend the exclude list for gempak files if needed
    if [[ "${RUN}" == "gfs" && ${current_date} -ge ${gempak_cutoff_date} && "${DO_GEMPAK}" == "YES" ]]; then
        # Provide the gempak exclude pattern(s)
        exclude_string+="${gempak_exclude_string}"
    fi

    # Check if the cycle completed successfully by looking at the rocoto log
    if [[ -f "${rocotolog}" ]]; then
        # TODO: This needs to be revamped to not look at the rocoto log.
        tail_log=$(tail -n 1 "${rocotolog}") || true
        # Test if the last line of rocotolog indicates success
        if [[ ${tail_log} =~ "This cycle is complete: Success" ]]; then
            YMD="${current_PDY}" HH="${current_cyc}" declare_from_tmpl \
                COMOUT_TOP:COM_TOP_TMPL
            if [[ -d "${COMOUT_TOP}" ]]; then
                IFS=", " read -r -a exclude_list <<< "${exclude_string}"
                remove_files "${COMOUT_TOP}" "${exclude_list[@]:-}"
            fi
            # Remove all rtofs directories in each RUN older than last_rtofs_date
            rtofs_dir="${ROTDIR}/rtofs.${current_PDY}"
            if [[ -d "${rtofs_dir}" && ${current_date} -lt ${rtofs_cutoff_date} ]]; then
                rm -rf "${rtofs_dir}"
            fi
        fi
    fi
done

# Remove archived gaussian files used for Fit2Obs in $VFYARC that are
# $FHMAX_FITS plus a delta before ${PDY}${cyc}. Touch existing archived
# gaussian files to prevent the files from being removed by automatic
# scrubber present on some machines.

if [[ "${RUN}" == "gfs" ]]; then
    fhmax=$((FHMAX_FITS + 36))
    RDATE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${fhmax} hours")
    verify_dir="${ROTDIR}/vrfyarch/${RUN}.${RDATE:0:8}"
    if [[ -d "${verify_dir}" ]]; then
        rm -rf "${verify_dir}"
    fi

    touch_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${FHMAX_FITS} hours")
    while ((touch_date < "${PDY}${cyc}")); do
        touch_PDY="${touch_date:0:8}"
        touch_cyc="${touch_date:8:2}"
        touch_dir="${ROTDIR}/vrfyarch/${RUN}.${touch_PDY}/${touch_cyc}"
        if [[ -d "${touch_dir}" ]]; then
            touch "${touch_dir}"/*
        fi
        touch_date=$(date --utc +%Y%m%d%H -d "${touch_PDY} ${touch_cyc} +6 hours")
    done
fi

# Remove $RUN.$rPDY for the older of GDATE or RDATE if it is empty
GDATE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${max_cleanup_max} hours")
RDATE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} -${FHMAX_GFS} hours")
if ((GDATE < RDATE)); then
    RDATE=${GDATE}
fi

deletion_target="${ROTDIR}/${RUN}.${RDATE:0:8}"
if [[ -d "${deletion_target}" ]]; then
    find "${deletion_target}" -maxdepth 0 -type d -empty -delete
fi

# sync and wait to avoid filesystem synchronization issues
sync && sleep 1
