#! /usr/bin/env bash
#
# Backward-compatibility shim for jjob_shell_setup.sh
#
# This script delegates to universal_wrapper.sh internals while
# preserving the original interface so that existing J-Jobs continue
# to work without per-job edits.
#
# Usage (unchanged from original):
#   source "${USHglobal}/jjob_shell_setup.sh"
#
# Handles:
#   - Sourcing utility functions (wait_for_file, dataroot_com_path, timer,
#       err_exit)
#   - Setting shell options (nullglob)
#   - Activating tracing (set -x)
#   - Setting up the postamble EXIT trap for script timing and cleanup
#   - Running setpdy.sh and sourcing PDY date variables
#
# Requires in environment:
#   HOMEglobal (mandatory)
#   USHglobal, start_time (defaulted here if not already set)
#
# Traces to: Requirement 6.9
#

# Determine the wrapper location (EXPDIR layout or dev/ layout)
_WRAPPER_DIR="${USHglobal:-${HOMEglobal}/ush}"
_UNIVERSAL_WRAPPER="${_WRAPPER_DIR}/universal_wrapper.sh"

if [[ -f "${_UNIVERSAL_WRAPPER}" ]]; then
    # Source only the shell-setup portion of the universal wrapper.
    export _UW_MODE="shell_setup"
    # shellcheck disable=SC1090
    source "${_UNIVERSAL_WRAPPER}"
    unset _UW_MODE
else
    # Fallback: provide the original implementation directly.
    # This path is used when universal_wrapper.sh has not yet been
    # deployed (e.g., during development or in legacy EXPDIRs).

    ##############################################
    # Shell defaults (allow sourcing without jjob_standard_vars.sh)
    ##############################################
    export USHglobal="${USHglobal:-${HOMEglobal}/ush}"
    export start_time=${start_time:-$(date +%s)}
    export _calling_script=${_calling_script:-$(basename "${BASH_SOURCE[1]}")}

    ##############################################
    # Utility functions
    ##############################################
    source "${USHglobal}/wait_for_file.sh"
    source "${USHglobal}/dataroot_com_path.sh"
    source "${USHglobal}/timer.sh"
    source "${USHglobal}/err_exit.sh"
    shopt -s nullglob # Allow null globs instead of treating * as literal

    ##############################################
    # Shell options
    ##############################################
    export SHELLOPTS

    ##############################################
    # Create and enter the working directory
    ##############################################
    source "${USHglobal}/setup_data_dir.sh" "${DATA}"

    # Activate tracing
    set -x

    ##############################################
    # Exit trap: run postamble on exit to report elapsed time and clean up
    ##############################################
    # shellcheck disable=SC2064
    trap "${USHglobal}/postamble.sh ${start_time}" EXIT

    ##############################################
    # Temporal variables: PDY, PDYm#, PDYp# (via setpdy.sh)
    ##############################################
    # setpdy.sh may not be available in all environments; failures are non-fatal
    setpdy.sh || true
    source ./PDY || true
fi
