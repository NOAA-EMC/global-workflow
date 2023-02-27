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
    # Each argument must have a corresponding template with the name ${ARG}_TMPL.
    #
    # Accepts as options all the same options the bash built-in `declare` allows except
    #  -g, which is assumed, and -p. These options are passed to `declare`.
    #
    # Syntax:
    #   generate_com [-aAfFilrtux] $var1[:$tmpl1] [$var2[:$tmpl2]] [...]]
    #
    #   options: Same function as the bash `declare` built-in
    #   var1, var2, etc: Variable names whose values will be generated from a template
    #                    and declared
    #   tmpl1, tmpl2, etc: Specify the template to use (default is "${var}_TMPL")
    #
    local opts="-g"
    local OPTIND=1
    while getopts "aAfFilrtux" option; do
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
        value=$(echo "${!template}" | envsubst)
        # shellcheck disable=SC2086
        declare ${opts} "${com_var}"="${value}"
    done
}
# shellcheck disable=
declare -xf generate_com

# Turn on our settings
set_strict
set_trace
