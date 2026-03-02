#! /usr/bin/env bash

#######
# Shell environment setup for all J-Jobs.
#
# Source after jjob_standard_vars.sh at the top of every J-Job.
#
# Handles:
#   - Sourcing bash_utils.sh for shared shell utility functions
#   - Setting shell options (nullglob)
#   - Sourcing set_strict.sh and exporting strict mode/trace functions
#   - Activating strict mode (set -eu) and tracing (set -x)
#   - Setting up the postamble EXIT trap for script timing and cleanup
#   - Running setpdy.sh and sourcing PDY date variables
#
# Requires in environment (set by jjob_standard_vars.sh):
#   HOMEgfs, USHglobal, start_time
#######

##############################################
# Bash utility functions and shell options
##############################################
source "${HOMEgfs}/ush/bash_utils.sh"
shopt -s nullglob # Allow null globs instead of treating * as literal

##############################################
# Shell options, strict mode, and tracing
##############################################
source "${USHglobal}/set_strict.sh"
export SHELLOPTS

# Export strict mode, trace, postamble, and err_exit functions to subshells
declare -xf set_strict
declare -xf unset_strict
declare -xf set_trace
declare -xf postamble
declare -xf err_exit

# Activate strict mode and tracing
set_strict
set_trace

##############################################
# Exit trap: run postamble on exit to report elapsed time and clean up
##############################################
source "${USHglobal}/postamble.sh"
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
