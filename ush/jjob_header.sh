#! /usr/bin/env bash
#
# Universal header for global j-jobs
#
# Sets up and completes actions common to all j-jobs:
# - Creates and moves to $DATA after removing any
#     existing one unless $WIPE_DATA is set to "NO"
# - Runs `setpdy.sh`
# - Sources configs provided as arguments
# - Sources machine environment script
# - Defines a few other variables
#
# The job name for the environment files should be passed
#   in using the `-e` option (required). Any config files
#   to be sourced should be passed in as an argument to
#   the `-c` option. For example:
#   ```
#   jjob_header.sh -e "fcst" -c "base fcst"
#   ```
#   Will source `config.base` and `config.fcst`, then pass
#   `fcst` to the ${machine}.env script.
#
# Script requires the following variables to already be
#   defined in the environment:
#   - $HOMEgfs
#   - $DATAROOT (unless $DATA is overriden)
#   - $jobid
#   - $PDY
#   - $cyc
#   - $machine
#
# Additionally, there are a couple of optional settings that
#   can be set before calling the script:
#   - $EXPDIR       : Override the default $EXPDIR
#                     [default: ${HOMEgfs}/parm/config]
#   - $DATA         : Override the default $DATA location
#                     [default: ${DATAROOT}/${jobid}]
#   - $WIPE_DATA    : Set whether to delete any existing $DATA
#                     [default: "YES"]
#   - $pid          : Override the default process id
#                     [default: $$]

_calling_script="${BASH_SOURCE[1]}"
source "${HOMEgfs}/ush/preamble.sh"

OPTIND=1
while getopts "c:e:" option; do
    case "${option}" in
        c)  read -ra configs <<< "${OPTARG}" ;;
        e)  env_job=${OPTARG} ;;
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
shift $((OPTIND-1))

if [[ -z ${env_job} ]]; then
    export err=1
    err_exit "[${BASH_SOURCE[0]}]: Must specify a job name with -e"
fi

##############################################
# make temp directory
##############################################
export DATA=${DATA:-"${DATAROOT}/${jobid}"}
if [[ ${WIPE_DATA:-YES} == "YES" ]]; then
    rm -rf "${DATA}"
fi
mkdir -p "${DATA}"
if ! cd "${DATA}"; then
  export err=1
  err_exit "[${BASH_SOURCE[0]}]: ${DATA} does not exist"
fi


##############################################
# Determine Job Output Name on System
##############################################
export pid="${pid:-$$}"
export pgmout="OUTPUT.${pid}"
export pgmerr=errfile
# TODO: remove this when going to production
# Needs to be set for err_chk/err_exit
export pgm=${pgm:-}


##############################################
# Run setpdy and initialize PDY variables
##############################################
export cycle="t${cyc}z"
setpdy.sh || true
source ./PDY || true


#############################
# Source relevant config files
#############################
export EXPDIR="${EXPDIR:-${HOMEgfs}/parm/config}"
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
source "${HOMEgfs}/env/${machine}.env" "${env_job}" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
   err_exit "[${BASH_SOURCE[0]}]: Error while sourcing machine environment ${machine}.env for job ${env_job}"
fi
