#! /usr/bin/env bash
#######
# Source this file to set strict mode in a script.
#   source "${USHgfs}/set_strict.sh"
#######

if [[ ${STRICT:-"YES"} == "YES" ]]; then
    # Exit on error or undefined variable
    set -eu
    # Exit on error in a pipeline (e.g. if a command in "cmd | cmd2" fails)
    # TODO Enable -o pipefail tests as well
    # set -o pipefail
fi
