#! /usr/bin/env bash

#######
# Standard environment variables for all J-Jobs.
#
# Source at the top of every J-Job (via jjob_init.sh):
#   source "${HOMEgfs}/dev/ush/jjob_init.sh"
#
# Sets variables defined in NCO HPC Implementation Standards Table 1:
#   https://github.com/NCO-HPC/nws-hpc-standards/blob/develop/docs/standards.rst
#
# Variables set here:
#   PS4                                         (debug trace format)
#   USHglobal, FIXglobal, PARMglobal,
#     SCRIPTSglobal                             (global directory paths)
#   pid, pgmout, pgmerr, pgm                   (job output variables)
#   envir, KEEPDATA, SENDECF, SENDDBN,
#     SENDDBN_NTC, DBNROOT                      (run environment and control)
#   DATA                                        (working directory)
#   cycle, PDY, PDYm#, PDYp#                   (temporal variables)
#
# Requires in environment (set by job card / batch system):
#   HOMEgfs, DATAROOT, jobid, cyc
#######

##############################################
# Debug trace format
##############################################
declare -x PS4='+ $(basename ${BASH_SOURCE[0]:-${FUNCNAME[0]:-"Unknown"}})[${LINENO}]'

##############################################
# Standard global directory paths
##############################################
export USHglobal="${HOMEgfs}/ush"
export FIXglobal="${HOMEgfs}/fix"
export PARMglobal="${HOMEgfs}/parm"
export SCRIPTSglobal="${HOMEgfs}/scripts"

##############################################
# Shell options, strict mode, and tracing
##############################################
source "${USHglobal}/set_strict.sh"
export SHELLOPTS

# Export functions to subshells
declare -xf set_strict
declare -xf unset_strict
declare -xf set_trace
declare -xf postamble
declare -xf err_exit

# Activate strict mode and tracing
set_strict
set_trace

##############################################
# Script timing and exit trap
##############################################
export start_time=$(date +%s)
source "${USHglobal}/postamble.sh"
# shellcheck disable=SC2064
trap "postamble ${start_time}" EXIT

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
export cycle="t${cyc}z"
unset_strict
setpdy.sh || true
source ./PDY || true
set_strict
