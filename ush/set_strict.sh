#! /usr/bin/env bash
#######
# Source this file to set strict mode in a script.
#   source "${USHglobal}/set_strict.sh"
#
# STRICT=YES (default): exits immediately on undefined variable, non-zero
#   command status, or failed pipe component.
# STRICT=NO: logs a warning on non-zero command status or failed pipe
#   component but continues execution. Undefined variables silently expand
#   to empty string (bash has no native warn-only mode for unset variables).
#######

if [[ ${STRICT:-"YES"} == "YES" ]]; then
    # Exit on error or undefined variable
    set -eu
    # Exit on error in a pipeline (e.g. if a command in "cmd | cmd2" fails)
    set -o pipefail
else
    # Ensure pipeline exit status reflects any failed component
    set -o pipefail
    # Log a warning when any command or pipeline returns non-zero but continue
    trap 'echo "WARNING: command exited with status $? at line ${LINENO} of ${BASH_SOURCE[0]:-${0}}" >&2' ERR
fi
