#! /usr/bin/env bash

#######
# Standard environment variables for all J-Jobs.
#
# Source at the top of every J-Job before jjob_shell_setup.sh:
#   source "${HOMEglobal}/ush/jjob_standard_vars.sh"
#   source "${HOMEglobal}/ush/jjob_shell_setup.sh"
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
#   cycle                                       (temporal variable; PDY, PDYm#,
#                                                PDYp# set in jjob_shell_setup.sh
#                                                via setpdy.sh)
#
# Requires in environment (set by job card / batch system):
#   HOMEglobal, DATAROOT, jobid, cyc
#######

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
