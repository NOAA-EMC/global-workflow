#! /usr/bin/env bash

#######
# Preamble script to be SOURCED at the beginning of every script. Sets 
#   useful PS4 and optionally turns on set -x and set -eu. Also sets up 
#   crude script timing and provides a postamble that runs on exit.
#
# Syntax:
#   preamble.sh [id]
#   
#   Aruguments:
#     id: Optional identifier string. Use when running the same script 
#           multiple times in the same job (e.g. MPMD)
#
# Input environment variables:
#   TRACE (YES/NO): Whether to echo every command (set -x) [default: "YES"]
#   STRICT (YES/NO): Whether to exit immediately on error or undefined variable
#     (set -eu) [default: "YES"]
#   POSTAMBLE_CMD (empty/set): A command to run at the end of the job
#     [default: empty]
#
#######
set +x
if (( $# > 0 )); then
    id="(${1})"
else
    id=""
fi

# Record the start time so we can calculate the elapsed time later
start_time=$(date +%s)

# Get the base name of the calling script
_calling_script=$(basename "${BASH_SOURCE[1]}")

# Announce the script has begun
start_time_human=$(date -d"@${start_time}" -u)
echo "Begin ${_calling_script} at ${start_time_human}"

declare -rx PS4='+ $(basename ${BASH_SOURCE[0]:-${FUNCNAME[0]:-"Unknown"}})[${LINENO}]'"${id}: "

set_strict() {
    if [[ ${STRICT:-"YES"} == "YES" ]]; then
        # Exit on error and undefined variable
        set -eu
    fi
}

set_trace() {
    # Print the script name and line number of each command as it is 
    #   executed when using trace. 
    if [[ ${TRACE:-"YES"} == "YES" ]]; then
        set -x
    fi
}

postamble() {
    #
    # Commands to execute when a script ends. 
    #
    # Syntax:
    #   postamble script start_time rc
    #
    #   Arguments:
    #     script: name of the script ending
    #     start_time: start time of script (in seconds)
    #     rc: the exit code of the script
    #

    set +x
    script="${1}"
    start_time="${2}"
    rc="${3}"

    # Execute postamble command
    #
    # Commands can be added to the postamble by appending them to $POSTAMBLE_CMD:
    #    POSTAMBLE_CMD="new_thing; ${POSTAMBLE_CMD:-}" # (before existing commands)
    #    POSTAMBLE_CMD="${POSTAMBLE_CMD:-}; new_thing" # (after existing commands)
    #
    # Always use this form so previous POSTAMBLE_CMD are not overwritten. This should
    #   only be used for commands that execute conditionally (i.e. on certain machines
    #   or jobs). Global changes should just be added to this function.
    # These commands will be called when EACH SCRIPT terminates, so be mindful. Please
    #   consult with global-workflow CMs about permanent changes to $POSTAMBLE_CMD or
    #   this postamble function.
    #

    if [[ -v 'POSTAMBLE_CMD' ]]; then
      ${POSTAMBLE_CMD}
    fi

    # Calculate the elapsed time
    end_time=$(date +%s)
    end_time_human=$(date -d@"${end_time}" -u +%H:%M:%S)
    elapsed_sec=$((end_time - start_time))
    elapsed=$(date -d@"${elapsed_sec}" -u +%H:%M:%S)

    # Announce the script has ended, then pass the error code up
    echo "End ${script} at ${end_time_human} with error code ${rc:-0} (time elapsed: ${elapsed})"
    exit "${rc}"
}

# Place the postamble in a trap so it is always called no matter how the script exits
# Shellcheck: Turn off warning about substitions at runtime instead of signal time
# shellcheck disable=SC2064
trap "postamble ${_calling_script} ${start_time} \$?" EXIT
# shellcheck disable=

function generate_com() {
    #
    # Generate a list COM variables from a template by substituting in env variables.
    #
    # Each argument must have a corresponding template with the name ${ARG}_TMPL. Any 
    #   variables in the template are replaced with their values. Undefined variables
    #   are just removed without raising an error.
    #
    # Accepts as options `-r` and `-x`, which do the same thing as the same options in
    #   `declare`. Variables are automatically marked as `-g` so the variable is visible
    #   in the calling script.
    #
    # Syntax:
    #   generate_com [-rx] $var1[:$tmpl1] [$var2[:$tmpl2]] [...]]
    #
    #   options:
    #       -r: Make variable read-only (same as `decalre -r`)
    #       -x: Mark variable for export (same as `declare -x`)
    #   var1, var2, etc: Variable names whose values will be generated from a template
    #                    and declared
    #   tmpl1, tmpl2, etc: Specify the template to use (default is "${var}_TMPL")
    #
    #   Examples:
    #       # Current cycle and RUN, implicitly using template COM_ATMOS_ANALYSIS_TMPL
    #       YMD=${PDY} HH=${cyc} generate_com -rx COM_ATMOS_ANALYSIS
    #
    #       # Previous cycle and gdas using an explicit template
    #       RUN=${GDUMP} YMD=${gPDY} HH=${gcyc} generate_com -rx \
    #           COM_ATMOS_HISTORY_PREV:COM_ATMOS_HISTORY_TMPL
    #
    #       # Current cycle and COM for first member
    #       MEMDIR='mem001' YMD=${PDY} HH=${cyc} generate_com -rx COM_ATMOS_HISTORY
    #
    if [[ ${DEBUG_WORKFLOW:-"NO"} == "NO" ]]; then set +x; fi
    local opts="-g"
    local OPTIND=1
    while getopts "rx" option; do
        opts="${opts}${option}"
    done
    shift $((OPTIND-1))

    for input in "$@"; do
        IFS=':' read -ra args <<< "${input}"
        local com_var="${args[0]}"
        local template
        local value
        if (( ${#args[@]} > 1 )); then
            template="${args[1]}"
        else
            template="${com_var}_TMPL"
        fi
        if [[ ! -v "${template}" ]]; then
            echo "FATAL ERROR in generate_com: Requested template ${template} not defined!"
            exit 2
        fi
        value=$(echo "${!template}" | envsubst)
        # shellcheck disable=SC2086
        declare ${opts} "${com_var}"="${value}"
        echo "generate_com :: ${com_var}=${value}"
    done
    set_trace
}
# shellcheck disable=
declare -xf generate_com

function wait_for_file() {
    #
    # Wait for a file to exist and return the status.
    #
    # Checks if a file exists periodically up to a maximum number of attempts. When the file
    #   exists or the limit is reached, the status is returned (0 if the file exists,1 if it
    #   does not). This allows it to be used as a conditional to handle missing files.
    #
    # Syntax:
    #   wait_for_file file_name [sleep_interval [max_tries]]
    #
    #     file_name:      File to check the existence of (must be readable)
    #     sleep_interval: Time to wait between each check (in seconds) [default: 60]
    #     max_tries:      The maximum number of checks to make [default: 100]
    #
    # Example:
    #     ```
    #     file_name=/path/to/foo
    #     sleep_interval=60
    #     max_tries=30
    #     if wait_for_file; then
    #       echo "FATAL ERROR: ${file_name} still does not exist after waiting one-half hour."
    #       exit 1
    #     fi
    #     # Code that depends on file existing
    #     ```
    #
    set +x
    local file_name=${1:?"wait_for_file() requires a file name"}
    local sleep_interval=${2:-60}
    local max_tries=${3:-100}

    for (( iter=0; iter<max_tries; iter++ )); do
        if [[ -r ${file_name} ]]; then
            set_trace
            return 0
        fi
        sleep "${sleep_interval}"
    done
    set_trace
    return 1
}
declare -xf wait_for_file

function detect_py_ver() {
    # 
    # Returns the major.minor version of the currently active python executable
    #
    regex="[0-9]+\.[0-9]+"
    # shellcheck disable=SC2312
    if [[ $(python --version) =~ ${regex} ]]; then
	    echo "${BASH_REMATCH[0]}"
    else
	    echo "FATAL ERROR: Could not detect the python version"
	    exit 1
    fi
}
# shellcheck disable=
declare -xf detect_py


# Turn on our settings
set_strict
set_trace
