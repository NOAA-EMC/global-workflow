#! /usr/bin/env bash
#
# Universal header for global j-jobs
#
# Sets up and completes actions common to all j-jobs:
# - Creates and moves to $DATA
# - Runs `setpdy.sh`
# - Sources configs provided as arguments
# - Sources machine environment script
# - Defines a few other variables
#
# The list of config files to source should be provided
#   by providing the names as arguments, i.e.
#   `jjob_header.sh base fcst` will source `config.base`
#   and `config.fcst` from the $EXPDIR.
#
# Script requires the following variables to already be
#   defined in the environment:
#   - $HOMEgfs
#   - $DATAROOT
#   - $jobid
#   - $PDY
#   - $cyc
#   - $machine
#

if (( $# < 1 )); then
    echo "FATAL: Must specify a job name"
fi
configs=("$@")

##############################################
# make temp directory
##############################################
export DATA=${DATA:-"${DATAROOT}/${jobid}"}
mkdir -p "${DATA}"
cd "${DATA}" || ( echo "FATAL: ${DATA} does not exist"; exit 1 )


##############################################
# Run setpdy and initialize PDY variables
##############################################
export cycle="t${cyc}z"
setpdy.sh
source ./PDY


##############################################
# Determine Job Output Name on System
##############################################
export pid="${pid:-$$}"
export pgmout="OUTPUT.${pid}"
export pgmerr=errfile


#############################
# Source relevant config files
#############################
export EXPDIR="${EXPDIR:-${HOMEgfs}/parm/config}"
for config in "${configs[@]}"; do
    source "${EXPDIR}/config.${config}"
    status=$?
    if (( status != 0 )); then
    	exit "${status}"
    fi
done


##########################################
# Source machine runtime environment
##########################################
source "${HOMEgfs}/env/${machine}.env" "${job}"
status=$?
if (( status != 0 )); then
	exit "${status}"
fi
