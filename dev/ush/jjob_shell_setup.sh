#! /usr/bin/env bash

#######
# Shell environment setup for all J-Jobs.
#
# Source after jjob_standard_vars.sh at the top of every J-Job.
#
# Handles:
#   - Sourcing utility functions (wait_for_file, dataroot_com_path, timer,
#       err_exit, set_strict, postamble)
#   - Setting shell options (nullglob)
#   - Each utility script exports its own functions via declare -xf
#   - Activating strict mode (set -eu) and tracing (set -x)
#   - Setting up the postamble EXIT trap for script timing and cleanup
#   - Running setpdy.sh and sourcing PDY date variables
#
# Requires in environment:
#   HOMEglobal (mandatory)
#   USHglobal, start_time (defaulted here if not already set)
#######

##############################################
# Shell defaults (allow sourcing without jjob_standard_vars.sh)
##############################################
# Ensure USHglobal is set for scripts that source this file directly
# (e.g. preamble.sh callers such as run_mpmd.sh)
export start_time=${start_time:-$(date +%s)}
_calling_script=${_calling_script:-$(basename "${BASH_SOURCE[1]}")}

##############################################
# Utility functions
##############################################
source "${HOMEglobal}/dev/ush/wait_for_file.sh"
source "${HOMEglobal}/dev/ush/dataroot_com_path.sh"
source "${HOMEglobal}/dev/ush/timer.sh"
source "${HOMEglobal}/dev/ush/err_exit.sh"
shopt -s nullglob # Allow null globs instead of treating * as literal

##############################################
# Shell options, strict mode, and tracing
##############################################
source "${HOMEglobal}/dev/ush/set_strict_trace.sh"
export SHELLOPTS

# Activate strict mode and tracing
set_strict
set_trace

##############################################
# Exit trap: run postamble on exit to report elapsed time and clean up
##############################################
source "${HOMEglobal}/dev/ush/postamble.sh"
# shellcheck disable=SC2064
trap "postamble ${start_time}" EXIT

##############################################
# Temporal variables: PDY, PDYm#, PDYp# (via setpdy.sh)
##############################################
# setpdy.sh may not be available in all environments; failures are non-fatal
unset_strict
setpdy.sh || true
source ./PDY || true
set_strict
