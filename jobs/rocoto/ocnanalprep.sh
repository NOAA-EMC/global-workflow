#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source GDASApp modules
module purge
module use "${HOMEgfs}/sorc/gdas.cd/modulefiles"
module load "GDAS/${machine,,}"

export job="ocnanalprep"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGDAS_GLOBAL_OCEAN_ANALYSIS_PREP
status=$?
exit "${status}"
