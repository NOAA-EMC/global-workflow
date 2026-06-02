#! /usr/bin/env bash
#
# Backward-compatibility shim for jjob_standard_vars.sh
#
# This script delegates to universal_wrapper.sh internals while
# preserving the original interface so that existing J-Jobs continue
# to work without per-job edits.
#
# Usage (unchanged from original):
#   source "${HOMEglobal}/ush/jjob_standard_vars.sh"
#
# Sets variables defined in NCO HPC Implementation Standards Table 1:
#   PS4, USHglobal, FIXglobal, PARMglobal, SCRIPTSglobal,
#   pid, pgmout, pgmerr, pgm, envir, KEEPDATA, SENDECF, SENDDBN,
#   SENDDBN_NTC, DBNROOT, DATA, cycle
#
# Requires in environment (set by job card / batch system):
#   HOMEglobal, DATAROOT, jobid, cyc
#
# Traces to: Requirement 6.9
#

# Determine the wrapper location (EXPDIR layout or dev/ layout)
_WRAPPER_DIR="${HOMEglobal}/ush"
_UNIVERSAL_WRAPPER="${_WRAPPER_DIR}/universal_wrapper.sh"

if [[ -f "${_UNIVERSAL_WRAPPER}" ]]; then
    # Source only the standard-vars portion of the universal wrapper.
    export _UW_MODE="standard_vars"
    # shellcheck disable=SC1090
    source "${_UNIVERSAL_WRAPPER}"
    unset _UW_MODE
else
    # Fallback: provide the original implementation directly.
    # This path is used when universal_wrapper.sh has not yet been
    # deployed (e.g., during development or in legacy EXPDIRs).

    ##############################################
    # Script timing: record start time and announce the job has begun
    ##############################################
    start_time=$(date +%s)
    export start_time
    _start_time_human=$(date -d"@${start_time}" -u +%H:%M:%S)
    _calling_script=${_calling_script:-$(basename "${BASH_SOURCE[1]}")}
    echo "Begin ${_calling_script} at ${_start_time_human}"

    ##############################################
    # Debug trace format
    ##############################################
    declare -x PS4='+ $(basename ${BASH_SOURCE[0]:-${FUNCNAME[0]:-"Unknown"}})[${LINENO}]'

    ##############################################
    # Standard global directory paths
    ##############################################
    export USHglobal="${HOMEglobal}/ush"
    export FIXglobal="${HOMEglobal}/fix"
    export PARMglobal="${HOMEglobal}/parm"
    export SCRIPTSglobal="${HOMEglobal}/scripts"

    ##############################################
    # Job output variables
    ##############################################
    export pid="${pid:-$$}"
    export pgmout="OUTPUT.${pid}"
    export pgmerr=errfile
    export pgm="${pgm:-}"

    ##############################################
    # Run environment and control variables
    ##############################################
    export envir=${envir:-"prod"}
    export KEEPDATA=${KEEPDATA:-"NO"}
    export SENDECF=${SENDECF:-"NO"}
    export SENDDBN=${SENDDBN:-"NO"}
    export SENDDBN_NTC=${SENDDBN_NTC:-"NO"}
    export DBNROOT=${DBNROOT:-${UTILROOT:-}/fakedbn}

    ##############################################
    # Working directory
    ##############################################
    export DATA="${DATA:-${DATAROOT}/${jobid}}"

    ##############################################
    # Temporal variables
    ##############################################
    # cycle is set here; PDY, PDYm#, PDYp# are set in jjob_shell_setup.sh via setpdy.sh
    export cycle="t${cyc}z"
fi
