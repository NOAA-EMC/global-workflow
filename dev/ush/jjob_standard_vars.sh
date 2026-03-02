#! /usr/bin/env bash

#######
# Standard environment variables for all J-Jobs (NCO standards Table 1).
#
# Source at the top of every J-Job (via jjob_init.sh):
#   source "${HOMEgfs}/dev/ush/jjob_init.sh"
#
# Sets only variables defined in NCO HPC Implementation Standards Table 1:
#   https://github.com/NCO-HPC/nws-hpc-standards/blob/develop/docs/standards.rst#standard-variables-formats-and-utilities
#
# Variables set here:
#   envir, KEEPDATA, SENDECF, SENDDBN, SENDDBN_NTC, DBNROOT  (env/run control)
#   DATA                                                        (working directory)
#   cycle, PDY, PDYm#, PDYp#                                   (temporal variables)
#
# Requires in environment (set by job card / batch system):
#   HOMEgfs, DATAROOT, jobid, cyc
#######

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
