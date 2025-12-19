#!/usr/bin/env bash

#######################################
# Display Rocoto workflow status for a given experiment
# Globals:
#   PWD
# Arguments:
#   -e expdir : Experiment directory path (optional, defaults to PWD)
#   -s        : Display summary view (optional flag)
# Outputs:
#   Writes Rocoto status information to stdout
# Returns:
#   0 on success, 1 on error
# Usage:
#   gw_expstat [-e /path/to/expdir] [-s]
#######################################
gw_expstat() {
    local expdir=""
    local summarize_flag=""

    while getopts "e:s" opt; do
        case "${opt}" in
            e)
                expdir="${OPTARG}"
                ;;
            s)
                summarize_flag="--summary"
                ;;
            *)
                echo "Usage: gw_expstat [-e expdir] [-s]"
                return 1
                ;;
        esac
    done
    shift $((OPTIND - 1))

    # Default to current directory name if pslot not specified
    if [[ -z "${expdir}" ]]; then
        expdir="${PWD}"
        pslot=$(basename "${PWD}")
    else
        pslot=$(basename "${expdir}")
    fi
    if [[ ! -f "${expdir}/${pslot}.xml" ]]; then
        echo "FATAL ERROR: '${pslot}.xml' does not exist in '${expdir}', ABORT!"
        return 1
    else
        rocotostat -w "${expdir}/${pslot}.xml" -d "${expdir}/${pslot}.db" -v 10 "${summarize_flag}"
    fi
}

#######################################
# Pad a string with characters on left and right to reach desired width
# This is a private helper function for formatting output
# Globals:
#   None
# Arguments:
#   str       : The string to pad
#   max_width : Total desired width including padding
#   pad_char  : Character to use for padding (optional, defaults to '#')
# Outputs:
#   Writes padded string to stdout
# Returns:
#   None
# Usage:
#   _gw_pad_str "My String" 70 "#"
#######################################
_gw_pad_str() {
    local str="${1}"
    local max_width="${2}"
    local pad_char="${3:-#}"
    local str_len pad_len left_pad right_pad left_padding right_padding
    str_len=${#str}
    pad_len=$((max_width - str_len))
    if [[ ${pad_len} -le 0 ]]; then
        echo "${str}"
        return
    fi

    left_pad=$((pad_len / 2))
    right_pad=$((pad_len - left_pad)) # Handle odd padding lengths
    left_padding=$(printf "%*s" "${left_pad}" "" | tr ' ' "${pad_char}")
    right_padding=$(printf "%*s" "${right_pad}" "" | tr ' ' "${pad_char}")
    printf "%s %s %s\n" "${left_padding}" "${str}" "${right_padding}"
}

#######################################
# Display Rocoto status for all CI test experiments in RUNTESTS directory
# Checks all experiment directories and displays status summary for each.
# Warns if DEAD tasks are detected in active experiments.
# Globals:
#   None
# Arguments:
#   -r RUNTESTS : Path to CI RUNTESTS directory containing EXPDIR subdirectory
# Outputs:
#   Writes formatted status information for all experiments to stdout
#   Writes warnings to stdout for experiments with DEAD tasks
# Returns:
#   0 on success, 1 on error (e.g., missing XML files)
# Usage:
#   gw_cistat -r /path/to/RUNTESTS
#######################################
gw_cistat() {
    local RUNTESTS=""
    while getopts "r:" opt; do
        case "${opt}" in
            r)
                RUNTESTS="${OPTARG}"
                ;;
            *)
                echo "Usage: gw_cistat -r RUNTESTS"
                return 1
                ;;
        esac
    done
    shift $((OPTIND - 1))

    if [[ -z "${RUNTESTS}" ]]; then
        echo "FATAL ERROR: RUNTESTS directory argument is required"
        echo "Usage: gw_cistat -r RUNTESTS"
        return 1
    fi
    local expdir pslot
    local summary details nactive ndead
    for expdir in "${RUNTESTS}/EXPDIR/"*; do
        if [[ ! -d "${expdir}" ]]; then
            echo "Skipping '${expdir}' as it is not a experiment directory"
            continue
        fi
        pslot=${expdir##*/}
        if [[ ! -f "${expdir}/${pslot}.xml" ]]; then
            echo "FATAL ERROR: '${expdir}/${pslot}.xml' does not exist in '${RUNTESTS}/EXPDIR/${expdir}', ABORT!"
            return 1
        fi
        _gw_pad_str "${pslot}" 70 "#"
        summary=$(gw_expstat -e "${expdir}" -s)
        cat <<< "${summary}"
        nactive=$(grep -c "Active" <<< "${summary}")
        if [[ ${nactive} -gt 0 ]]; then
            details=$(gw_expstat -e "${expdir}")
            ndead=$(grep -c "DEAD" <<< "${details}")
            if [[ ${ndead} -gt 0 ]]; then
                echo "WARNING: There are ${ndead} DEAD tasks in experiment '${pslot}'"
            fi
        fi
    done
}
