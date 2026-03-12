#! /usr/bin/env bash

#######
# Defines the set_strict function for use in J-jobs and ex-scripts.
#
# Source this file to load the function into the current shell:
#   source "${USHglobal}/set_strict.sh"
#######

set_strict() {
    if [[ ${STRICT:-"YES"} == "YES" ]]; then
        # Exit on error or undefined variable
        set -eu
        # Exit on error in a pipeline (e.g. if a command in "cmd | cmd2" fails)
        set -o pipefail
    fi
}

declare -xf set_strict
