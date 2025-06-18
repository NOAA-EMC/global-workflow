#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_atmos_verfrad.sh
# Script description:  Runs data extract/validation for global radiance diag data
#
# Author:        Ed Safford       Org: NP23         Date: 2012-01-18
#
# Abstract: This script runs the data extract/validation portion of the
#           RadMon package.
#
# Condition codes
#       0 - no problem encountered
#      >0 - some problem encountered
#
################################################################################

# Do not exit on errors so that restricted data can be protected
set +eu

if [[ ! -s ${radstat} || ! -s ${biascr} ]]; then
   export err=1
   err_exit "Required data files ${radstat} and/or ${biascr} are missing!!"
fi

#------------------------------------------------------------------
#  Copy data files file to local data directory.
#  Untar radstat file.
#------------------------------------------------------------------

cpreq "${biascr}"  "./biascr.${PDY}${cyc}"
cpreq "${radstat}" "./radstat.${PDY}${cyc}"

tar -xvf "radstat.${PDY}${cyc}"
rm "radstat.${PDY}${cyc}"

#------------------------------------------------------------------
#  SATYPE is the list of expected satellite/instrument sources
#  in the radstat file. It should be stored in the $TANKverf
#  directory. If it isn't there then use the gdas fix copy. In all
#  cases write it back out to the radmon.$PDY directory. Add any
#  new sources to the list before writing back out.
#------------------------------------------------------------------

radstat_satype=$(ls d*ges* | awk -F_ '{ print $2 "_" $3 }')
if [[ "${VERBOSE}" = "YES" ]]; then
   echo "${radstat_satype}"
fi

echo satype_file = "${satype_file}"

#------------------------------------------------------------------
#  Get previous cycle's date, and look for the satype_file.  Using
#  the previous cycle will get us the previous day's directory if
#  the cycle being processed is 00z.
#------------------------------------------------------------------
if [[ ${cyc} = "00" ]]; then
   use_tankdir=${TANKverf_radM1}
else
   use_tankdir=${TANKverf_rad}
fi

echo satype_file = "${satype_file}"
export SATYPE=$(cat "${satype_file}")


#-------------------------------------------------------------
#  Update the SATYPE if any new sat/instrument was
#  found in $radstat_satype.  Write the SATYPE contents back
#  to $TANKverf/radmon.$PDY.
#-------------------------------------------------------------
satype_changes=0
new_satype=${SATYPE}
for type in ${radstat_satype}; do
   type_count=$(echo "${SATYPE}" | grep "${type}" | wc -l)

   if [[ ${type_count} -eq 0 ]]; then
      if [[ "${VERBOSE}" = "YES" ]]; then
         echo "Found ${type} in radstat file but not in SATYPE list.  Adding it now."
      fi
      satype_changes=1
      new_satype="${new_satype} ${type}"
   fi
done

if [[ ${satype_changes} -eq 1 ]]; then
   SATYPE=${new_satype}
fi

#------------------------------------------------------------------
# Rename the diag files and uncompress
#------------------------------------------------------------------
netcdf=0

for type in ${SATYPE}; do

   if [[ ${netcdf} -eq 0 && -e "diag_${type}_ges.${PDY}${cyc}.nc4.${Z}" ]]; then
      netcdf=1
   fi

   if [[ $(find . -maxdepth 1 -type f -name "diag_${type}_ges.${PDY}${cyc}*.${Z}" | wc -l) -gt 0 ]]; then
     mv "diag_${type}_ges.${PDY}${cyc}"*".${Z}" "${type}.${Z}"
     ${UNCOMPRESS} "./${type}.${Z}"
   else
     echo "WARNING: diag_${type}_ges.${PDY}${cyc}*.${Z} not available, skipping"
   fi

   if [[ ${USE_ANL} -eq 1 ]]; then
     file_count=$(find . -maxdepth 1 -type f -name "diag_${type}_anl.${PDY}${cyc}*.${Z}" | wc -l)
     if [[ ${file_count} -gt 0 ]]; then
       mv "diag_${type}_anl.${PDY}${cyc}"*".${Z}" "${type}_anl.${Z}"
       ${UNCOMPRESS} "./${type}_anl.${Z}"
     else
       echo "WARNING: diag_${type}_anl.${PDY}${cyc}*.${Z} not available, skipping"
     fi
   fi
done

export RADMON_NETCDF=${netcdf}


#------------------------------------------------------------------
#   Run the child scripts.
#------------------------------------------------------------------

"${USHgfs}/radmon_verf_angle.sh" && true
rc_angle=$?
"${USHgfs}/rstprod.sh"

# Allow all scripts to run.  Call err_exit at the end, after files are restricted.
if [[ ${rc_angle} -ne 0 ]]; then
   echo "FATAL ERROR: radmon_verf_angle.sh failed!"
fi

"${USHgfs}/radmon_verf_bcoef.sh" && true
rc_bcoef=$?
"${USHgfs}/rstprod.sh"

if [[ ${rc_bcoef} -ne 0 ]]; then
   echo "FATAL ERROR: radmon_verf_bcoef.sh failed!"
fi

"${USHgfs}/radmon_verf_bcor.sh" && true
rc_bcor=$?
"${USHgfs}/rstprod.sh"

if [[ ${rc_bcoef} -ne 0 ]]; then
   echo "FATAL ERROR: radmon_verf_bcor.sh failed!"
fi

"${USHgfs}/radmon_verf_time.sh" && true
rc_time=$?
"${USHgfs}/rstprod.sh"

if [[ ${rc_bcoef} -ne 0 ]]; then
   echo "FATAL ERROR: radmon_verf_time.sh failed!"
fi

#####################################################################
# Error handling

export err=$((rc_angle + rc_bcoef + rc_bcor + rc_time))

if [[ ${err} -ne 0 ]]; then
   err_exit "One or more radiance monitor subtasks failed!"
fi

exit 0
