#! /usr/bin/env bash

#######
# Defines set_strict, unset_strict, and set_trace functions for use in
# J-jobs and ex-scripts.
#
# Source this file to load the functions into the current shell:
#   source "${USHglobal}/set_strict_trace.sh"
#######

set_strict() {
    if [[ ${STRICT:-"YES"} == "YES" ]]; then
        # Exit on error or undefined variable
        set -eu
        # Exit on error in a pipeline (e.g. if a command in "cmd | cmd2" fails)
        set -o pipefail
    fi
}

unset_strict() {
    # Turn off strict mode
    set +eu
    set +o pipefail
}

set_trace() {
    # Print the script name and line number of each command as it is
    # executed when using trace.
    if [[ ${TRACE:-"YES"} == "YES" ]]; then
        set -x
    fi
}

declare -xf set_strict
declare -xf unset_strict
declare -xf set_trace
