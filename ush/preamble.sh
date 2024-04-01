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

source "${HOMEgfs}/ush/bash_functions.sh"

# Turn on our settings
set_strict
set_trace
