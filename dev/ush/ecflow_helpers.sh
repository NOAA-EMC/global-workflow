#!/usr/bin/env bash

#######################################
# Display ecFlow workflow status for a given experiment
# Globals:
#   PWD
# Arguments:
#   -e expdir : Experiment directory path (optional, defaults to PWD)
#   -s        : Display summary view (optional flag)
# Outputs:
#   Writes ecFlow status information to stdout
# Returns:
#   0 on success, 1 on error
# Usage:
#   gw_expstat [-e /path/to/expdir] [-s]
#######################################
gw_expstat() {
    local expdir=""
    local summarize_flag=""
    local OPTIND=1

    while getopts "e:s" opt; do
        case "${opt}" in
            e)
                expdir="${OPTARG}"
                ;;
            s)
                summarize_flag="yes"
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

    local ECF_PORT="${ECF_PORT:-3141}"

    # Check if ecFlow definition file exists
    if [[ ! -f "${expdir}/ecf/defs/${pslot}.def" ]]; then
        echo "FATAL ERROR: ecFlow definition '${pslot}.def' does not exist in '${expdir}/ecf/defs/', ABORT!"
        return 1
    fi

    if [[ "${summarize_flag}" == "yes" ]]; then
        ecflow_client --port "${ECF_PORT}" --stats 2>/dev/null || \
            echo "ecFlow server not running or suite not loaded"
    else
        ecflow_client --port "${ECF_PORT}" --get_state "/${pslot}" 2>/dev/null || \
            echo "ecFlow server not running or suite '${pslot}' not loaded"
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
# Display ecFlow status for all CI test experiments in RUNTESTS directory
# Checks all experiment directories and displays status summary for each.
# Warns if aborted tasks are detected in active experiments.
# Globals:
#   None
# Arguments:
#   -r RUNTESTS : Path to CI RUNTESTS directory containing EXPDIR subdirectory
# Outputs:
#   Writes formatted status information for all experiments to stdout
#   Writes warnings to stdout for experiments with aborted tasks
# Returns:
#   0 on success, 1 on error (e.g., missing definition files)
# Usage:
#   gw_cistat -r /path/to/RUNTESTS
#######################################
gw_cistat() {
    local RUNTESTS=""
    local OPTIND=1

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

    local ECF_PORT="${ECF_PORT:-3141}"
    local expdir pslot
    local state_output naborted

    for expdir in "${RUNTESTS}/EXPDIR/"*; do
        if [[ ! -d "${expdir}" ]]; then
            echo "Skipping '${expdir}' as it is not an experiment directory"
            continue
        fi
        pslot=${expdir##*/}
        if [[ ! -f "${expdir}/ecf/defs/${pslot}.def" ]]; then
            echo "FATAL ERROR: '${expdir}/ecf/defs/${pslot}.def' does not exist, ABORT!"
            return 1
        fi
        _gw_pad_str "${pslot}" 70 "#"
        state_output=$(ecflow_client --port "${ECF_PORT}" --get_state "/${pslot}" 2>/dev/null) || true
        if [[ -n "${state_output}" ]]; then
            echo "${state_output}" | grep -c "state:complete" | xargs -I{} echo "  Complete: {}"
            echo "${state_output}" | grep -c "state:active" | xargs -I{} echo "  Active: {}"
            echo "${state_output}" | grep -c "state:queued" | xargs -I{} echo "  Queued: {}"
            naborted=$(echo "${state_output}" | grep -c "state:aborted" || echo "0")
            echo "  Aborted: ${naborted}"
            if [[ "${naborted}" -gt 0 ]]; then
                echo "WARNING: There are ${naborted} ABORTED tasks in experiment '${pslot}'"
            fi
        else
            echo "  Suite '${pslot}' not loaded in ecFlow server"
        fi
    done
}
