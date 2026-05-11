#! /usr/bin/env bash

#######
# Shell environment setup for all J-Jobs.
#
# Source after jjob_standard_vars.sh at the top of every J-Job.
#
# Handles:
#   - Sourcing utility functions (wait_for_file, dataroot_com_path, timer,
#       err_exit, postamble)
#   - Setting shell options (nullglob)
#   - Each utility script exports its own functions via declare -xf
#   - Setting up the postamble EXIT trap for script timing and cleanup
#   - Running setpdy.sh and sourcing PDY date variables
#
# Requires in environment:
#   HOMEgfs (mandatory)
#   USHgfs, start_time (defaulted here if not already set)
#######

##############################################
# Shell defaults (allow sourcing without jjob_standard_vars.sh)
##############################################
# Ensure USHgfs is set for scripts that source this file directly
# (e.g. preamble.sh callers such as run_mpmd.sh)
export USHgfs="${USHgfs:-${HOMEgfs}/ush}"
export start_time=${start_time:-$(date +%s)}
_calling_script=${_calling_script:-$(basename "${BASH_SOURCE[1]}")}

##############################################
# Utility functions
##############################################
source "${USHgfs}/wait_for_file.sh"
source "${USHgfs}/dataroot_com_path.sh"
source "${USHgfs}/timer.sh"
source "${USHgfs}/err_exit.sh"
shopt -s nullglob # Allow null globs instead of treating * as literal

##############################################
# Shell options
##############################################
export SHELLOPTS
##############################################
# Create and enter the working directory
##############################################
source "${USHgfs}/setup_data_dir.sh"
setup_data_dir "${DATA}"

# Activate tracing
set -x

##############################################
# Exit trap: run postamble on exit to report elapsed time and clean up
##############################################
source "${USHgfs}/postamble.sh"
# shellcheck disable=SC2064
trap "postamble ${start_time} \$?" EXIT

##############################################
# Temporal variables: PDY, PDYm#, PDYp# (via setpdy.sh)
##############################################
# setpdy.sh may not be available in all environments; failures are non-fatal
setpdy.sh || true
source ./PDY || true
