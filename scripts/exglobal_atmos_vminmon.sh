#! /usr/bin/env bash

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

if [[ ! -s "${gsistat}" ]]; then

   export err=1
   err_exit "Required GSI statistics file is missing!"

fi

#-----------------------------------------------------------------------
#  Copy the $MINMON_SUFFIX.gnorm_data.txt file to the working directory
#  It's ok if it doesn't exist; we'll create a new one if needed.
#
#  Note:  The logic below is to accomodate two different data storage
#  methods.  Some parallels (and formerly ops) dump all MinMon data for
#  a given day in the same directory (if condition).  Ops now separates
#  data into ${cyc} subdirectories (elif condition).
#-----------------------------------------------------------------------
if [[ -s "${M_TANKverf}/gnorm_data.txt" ]]; then
   cpreq "${M_TANKverf}/gnorm_data.txt" gnorm_data.txt
elif [[ -s "${M_TANKverfM1}/gnorm_data.txt" ]]; then
   cpreq "${M_TANKverfM1}/gnorm_data.txt" gnorm_data.txt
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

#####################################################################
# Postprocessing

err=$(( rc_costs + rc_gnorms + rc_reduct ))
export err=${err}

if [[ ${err} -ne 0 ]]; then
   err_exit "One or more minimization monitor subjobs failed!!"
fi
