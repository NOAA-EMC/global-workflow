#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_vminmon.sh
# Script description:  Runs data extract/validation for GSI normalization diag data
#
# Author:        Ed Safford       Org: NP23         Date: 2015-04-10
#
# Abstract: This script runs the data extract/validation portion of the
#           MinMon package.
#
# Condition codes
#       0 - no problem encountered
#      >0 - some problem encountered
#
################################################################################

data_available=0

if [[ -s ${gsistat} ]]; then

   data_available=1

   #-----------------------------------------------------------------------
   #  Copy the $MINMON_SUFFIX.gnorm_data.txt file to the working directory
   #  It's ok if it doesn't exist; we'll create a new one if needed.
   #
   #  Note:  The logic below is to accomodate two different data storage
   #  methods.  Some parallels (and formerly ops) dump all MinMon data for
   #  a given day in the same directory (if condition).  Ops now separates
   #  data into ${cyc} subdirectories (elif condition).
   #-----------------------------------------------------------------------
   if [[ -s ${M_TANKverf}/gnorm_data.txt ]]; then
      ${NCP} "${M_TANKverf}/gnorm_data.txt" gnorm_data.txt
   elif [[ -s ${M_TANKverfM1}/gnorm_data.txt ]]; then
      ${NCP} "${M_TANKverfM1}/gnorm_data.txt" gnorm_data.txt
   fi


   #------------------------------------------------------------------
   #   Run the child sccripts.
   #------------------------------------------------------------------
   "${USHgfs}/minmon_xtrct_costs.pl" "${MINMON_SUFFIX}" "${PDY}" "${cyc}" "${gsistat}"
   rc_costs=$?
   echo "rc_costs = ${rc_costs}"

   "${USHgfs}/minmon_xtrct_gnorms.pl" "${MINMON_SUFFIX}" "${PDY}" "${cyc}" "${gsistat}"
   rc_gnorms=$?
   echo "rc_gnorms = ${rc_gnorms}"

   "${USHgfs}/minmon_xtrct_reduct.pl" "${MINMON_SUFFIX}" "${PDY}" "${cyc}" "${gsistat}"
   rc_reduct=$?
   echo "rc_reduct = ${rc_reduct}"

fi

#####################################################################
# Postprocessing

err=0
if [[ ${data_available} -ne 1 ]]; then
   err=1
elif [[ ${rc_costs} -ne 0 ]]; then
   err=${rc_costs}
elif [[ ${rc_gnorms} -ne 0 ]]; then
   err=${rc_gnorms}
elif [[ ${rc_reduct} -ne 0 ]]; then
   err=${rc_reduct}
fi

exit "${err}"

