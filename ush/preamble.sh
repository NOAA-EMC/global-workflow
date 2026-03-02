#! /usr/bin/env bash

#######
# Preamble script to be SOURCED at the beginning of every script. Sets
#   useful PS4 and optionally turns on set -x and set -eu. Also sets up
#   crude script timing and provides a postamble that runs on exit.
#
# Syntax:
#   preamble.sh
#
# Input environment variables:
#   TRACE (YES/NO): Whether to echo every command (set -x) [default: "YES"]
#   STRICT (YES/NO): Whether to exit immediately on error or undefined variable
#     (set -eu) [default: "YES"]
#   POSTAMBLE_CMD (empty/set): A command to run at the end of the job
#     [default: empty]
#   _calling_script: The name of the calling script (optional)
#
#######
set +x

# Record the start time so we can calculate the elapsed time later
start_time=$(date +%s)

# Get the base name of the calling script
_calling_script=${_calling_script:-$(basename "${BASH_SOURCE[1]}")}

# Announce the script has begun
start_time_human=$(date -d"@${start_time}" -u +%H:%M:%S)
echo "Begin ${_calling_script} at ${start_time_human}"


source "${HOMEgfs}/ush/set_strict.sh"

source "${HOMEgfs}/ush/postamble.sh"

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

# Place the postamble in a trap so it is always called no matter how the script exits
# Shellcheck: Turn off warning about substitions at runtime instead of signal time
# shellcheck disable=SC2064
trap "postamble ${_calling_script} ${start_time} \$?" EXIT
# shellcheck disable=

source "${HOMEgfs}/ush/bash_utils.sh"

# Turn on our settings
shopt -s nullglob # Allow null globs instead of treating * as literal
export SHELLOPTS
