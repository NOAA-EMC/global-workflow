#! /usr/bin/env bash
#
# Backward-compatibility shim for jjob_header.sh
#
# This script delegates to universal_wrapper.sh internals while
# preserving the original interface so that existing J-Jobs continue
# to work without per-job edits.
#
# Usage (unchanged from original):
#   source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"
#
# Traces to: Requirement 6.9
#

# Determine the wrapper location (EXPDIR layout or dev/ layout)
_WRAPPER_DIR="${HOMEglobal}/ush"
_UNIVERSAL_WRAPPER="${_WRAPPER_DIR}/universal_wrapper.sh"

if [[ -f "${_UNIVERSAL_WRAPPER}" ]]; then
    # Source only the header/config-loading portion of the universal wrapper.
    # The universal wrapper exports a function for header initialization
    # that handles config sourcing and environment loading.
    export _UW_MODE="header"
    # shellcheck disable=SC1090
    source "${_UNIVERSAL_WRAPPER}" "$@"
    unset _UW_MODE
else
    # Fallback: source the original implementation directly.
    # This path is used when universal_wrapper.sh has not yet been
    # deployed (e.g., during development or in legacy EXPDIRs).

    # Set calling script name so it logs the J-Job name rather than this header
    _calling_script=${_calling_script:-$(basename "${BASH_SOURCE[1]}")}

    # err_exit is needed for this header script's own error handling;
    # all other utilities are sourced by jjob_shell_setup.sh afterward
    source "${HOMEglobal}/ush/err_exit.sh"

    OPTIND=1
    while getopts "c:e:" option; do
        case "${option}" in
            c) read -ra configs <<< "${OPTARG}" ;;
            e) env_job=${OPTARG} ;;
            :)
                export err=1
                err_exit "[${BASH_SOURCE[0]}]: ${option} requires an argument"
                ;;
            *)
                export err=1
                err_exit "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
                ;;
        esac
    done
    shift $((OPTIND - 1))

    if [[ -z ${env_job:-} ]]; then
        export err=1
        err_exit "[${BASH_SOURCE[0]}]: Must specify a job name with -e"
    fi

    #############################
    # Source relevant config files
    #############################
    export EXPDIR="${EXPDIR:-${HOMEglobal}/dev/parm/config}"
    for config in "${configs[@]:-''}"; do
        source "${EXPDIR}/config.${config}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "[${BASH_SOURCE[0]}]: Unable to load config config.${config}"
        fi
    done

    ##########################################
    # Source machine runtime environment
    ##########################################
    source "${HOMEglobal}/env/${machine}.env" "${env_job}" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "[${BASH_SOURCE[0]}]: Error while sourcing machine environment ${machine}.env for job ${env_job}"
    fi
fi
