#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source GDASApp modules
module purge
module use "${HOMEgfs}/sorc/gdas.cd/modulefiles"
module load GDAS/"${machine,,}"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGDAS_GLOBAL_OCEAN_ANALYSIS_RUN
status=$?
exit "${status}"
