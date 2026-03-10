#! /usr/bin/env bash

#######
# Defines the unset_strict function for use in J-jobs and ex-scripts.
#
# Source this file to load the function into the current shell:
#   source "${USHglobal}/unset_strict.sh"
#######

unset_strict() {
    # Turn off strict mode
    set +eu
    set +o pipefail
}

declare -xf unset_strict
