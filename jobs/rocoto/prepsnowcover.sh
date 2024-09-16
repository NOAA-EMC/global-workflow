#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="prepsnowcover"
export jobid="${job}.$$"

###############################################################
# setup python path for ioda utilities
# shellcheck disable=SC2311
pyiodaPATH="${HOMEgfs}/sorc/gdas.cd/build/lib/python$(detect_py_ver)/"
gdasappPATH="${HOMEgfs}/sorc/gdas.cd/sorc/iodaconv/src:${pyiodaPATH}"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}:${gdasappPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_PREP_SNOWCOVER"
status=$?
exit "${status}"
