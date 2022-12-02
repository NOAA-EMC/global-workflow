#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_global_aero_analysis_run.sh
# Script description:  Runs the global aerosol analysis with FV3-JEDI
#
# Author: Cory Martin        Org: NCEP/EMC     Date: 2022-29-09
#
# Abstract: This script runs a global model aerosol analysis using FV3-JEDI
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: Orion
#
################################################################################
# run executable
export pgm=${JEDIVAREXE}
. prep_step
"${APRUN_AEROANL}" "${DATA}"/fv3jedi_var.x "${DATA}"/"${CDUMP}"aeroanl_"${CDATE}".yaml 1>&1 2>&2
export err=$?; err_chk
exit "${err}"
