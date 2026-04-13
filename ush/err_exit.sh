#! /usr/bin/env bash

#---------------------------------------------------------
# err_exit.sh
#
# Script to create error exit function.
# Taken from NCO prod_util v2.1.0.
#
# ABSTRACT: This script is to be used when a fatal error or condition has been
#   reached and you want to terminate the job.
#
# USAGE: To use this script one must export the following variables to the
#   script: jobid, SENDECF, pgm, pgmout, DATA.
#   One can provide a message for the logfile by passing it to the script as an argument.
#---------------------------------------------------------

err_exit() {
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
        timeout 30 ssh "${ECF_HOST}" "echo \"${msg2}\" >> ${ECF_JOBOUT:?}"
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

declare -xf err_exit
