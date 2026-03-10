#! /usr/bin/env bash

#######
# Defines the set_trace function for use in J-jobs and ex-scripts.
#
# Source this file to load the function into the current shell:
#   source "${USHglobal}/set_trace.sh"
#######

set_trace() {
    # Print the script name and line number of each command as it is
    # executed when using trace.
    if [[ ${TRACE:-"YES"} == "YES" ]]; then
        set -x
    fi
}

declare -xf set_trace
