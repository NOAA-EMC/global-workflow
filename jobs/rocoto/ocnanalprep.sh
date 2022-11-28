#! /usr/bin/env bash

source "${HOMEgfs}"/ush/preamble.sh

###############################################################
# Source GDASApp modules
declare -l hpc=${machine}
module purge
module use "${HOMEgfs}"/sorc/gdas.cd/modulefiles
module load GDAS/"${hpc}"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGDAS_GLOBAL_OCEAN_ANALYSIS_PREP
status=$?
exit "${status}"
