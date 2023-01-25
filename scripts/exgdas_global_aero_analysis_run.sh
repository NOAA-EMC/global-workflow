#!/bin/bash
################################################################################
# exgdas_global_aero_analysis_run.sh
#
# This script runs a global aerosol variational analysis with FV3-JEDI.
# It assumes the runtime directory has already been staged with the appropriate
# input files and YAML configuration (by the initialize script) before execution.
#
################################################################################
# run executable
export pgm=${JEDIVAREXE}
. prep_step
${APRUN_AEROANL} "${DATA}/fv3jedi_var.x" "${DATA}/${CDUMP}.t${cyc}z.aerovar.yaml" 1>&1 2>&2
export err=$?; err_chk
exit "${err}"
