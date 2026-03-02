#! /usr/bin/env bash

#######
# Preamble script to be SOURCED at the beginning of every execution script.
#
# Captures the calling script name for logging and defines the err_exit
# function used for fatal error handling across all scripts.
#
# Syntax:
#   source "${HOMEgfs}/ush/preamble.sh"
#
# Sets:
#   _calling_script: Base name of the script that sourced this file
#
# Requires in environment:
#   (none required; HOMEgfs used indirectly by err_exit callers)
#
# Note:
#   Shell strict mode (set -eu), tracing (set -x), postamble EXIT trap,
#   and bash utility sourcing are handled in jjob_shell_setup.sh for J-Jobs.
#######
set +x
# Get the base name of the calling script
_calling_script=${_calling_script:-$(basename "${BASH_SOURCE[1]}")}
echo "Sourced-based script: ${_calling_script}"

# TODO: Remove this when moving to operations
function err_exit() {
    # Taken from NCO prod_util v2.1.0
    # SCRIPT NAME:  err_exit
    #
    # ABSTRACT:  This script is to be used when a fatal error or condition
    # has been reached and you want to terminate the job.
    #
    # USAGE:  To use this script one must export the following variables to the
    # script: jobid, SENDECF, pgm, pgmout, DATA. One can provide
    # a message for the logfile by passing it to the script as an argument.

    # Do not fail in err_exit
    set +eux

    msg1=${*:-Job ${jobid} failed}
    if [[ -n "${pgm}" ]]; then
        msg1+=", ERROR IN ${pgm}"
    fi
    if [[ -n "${err}" ]]; then
        msg1+=" RETURN CODE ${err}"
    fi

    msg2="
    -------------------------------------------------------------
    -- FATAL ERROR: ${msg1}
    -- ABNORMAL EXIT at $(date) on ${HOSTNAME}
    -------------------------------------------------------------
    "

    >&2 echo "${msg2}"

    # list loaded modules
    module list
    >&2 echo ""

    >&2 echo "${msg1}"

    # list files in temporary working directory
    if [[ -n "${DATA}" ]]; then
        >&2 echo "${DATA}"
        >&2 ls -ltr "${DATA}"
    else
        >&2 echo "WARNING: DATA variable not defined"
    fi

    # save standard output
    if [[ -n "${pgmout}" ]]; then
        if [[ -s errfile ]]; then
            echo "----- contents of errfile -----" >> "${pgmout}"
            cat errfile >> "${pgmout}"
        fi
        >&2 cat "${pgmout}"
    elif [[ -s errfile ]]; then
        >&2 cat errfile
    fi

    # Write to ecflow log:
    if [[ "${SENDECF}" == "YES" ]]; then
        timeout 30 ecflow_client --msg "${ECF_NAME}: ${msg1}"
        timeout 30 ssh "${ECF_HOST}" "echo \"${msg}2\" >> ${ECF_JOBOUT:?}"
    fi

    # KILL THE JOB:
    if [[ "${SENDECF}" == "YES" ]]; then
        ecflow_client --kill="${ECF_NAME:?}"
    fi

    if [[ -n "${PBS_JOBID}" ]]; then
        qdel "${PBS_JOBID}"
    elif [[ -n "${SLURM_JOB_ID}" ]]; then
        scancel "${SLURM_JOB_ID}"
    fi
}
