#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

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

data_available=0

if [[ -s ${radstat} && -s ${biascr} ]]; then
   data_available=1

   #------------------------------------------------------------------
   #  Copy data files file to local data directory.
   #  Untar radstat file.
   #------------------------------------------------------------------

   ${NCP} "${biascr}"  "./biascr.${PDY}${cyc}"
   ${NCP} "${radstat}" "./radstat.${PDY}${cyc}"

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

      if (( type_count == 0 )); then
         if [[ "${VERBOSE}" = "YES" ]]; then
            echo "Found ${type} in radstat file but not in SATYPE list.  Adding it now."
         fi
         satype_changes=1
         new_satype="${new_satype} ${type}"
      fi
   done


   #------------------------------------------------------------------
   # Rename the diag files and uncompress
   #------------------------------------------------------------------
   netcdf=0

   for type in ${SATYPE}; do

      if (( netcdf == 0 )) && [[ -e "diag_${type}_ges.${PDY}${cyc}.nc4.${Z}" ]]; then
         netcdf=1
      fi

      if [[ $(find . -maxdepth 1 -type f -name "diag_${type}_ges.${PDY}${cyc}*.${Z}" | wc -l) -gt 0 ]]; then
        mv "diag_${type}_ges.${PDY}${cyc}"*".${Z}" "${type}.${Z}"
        ${UNCOMPRESS} "./${type}.${Z}"
      else
        echo "WARNING: diag_${type}_ges.${PDY}${cyc}*.${Z} not available, skipping"
      fi

      if [[ ${USE_ANL} -eq 1 ]]; then
        if [[ $(find . -maxdepth 1 -type f -name "diag_${type}_anl.${PDY}${cyc}*.${Z}" | wc -l) -gt 0 ]]; then
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
    "${USHgfs}/radmon_verf_angle.sh"
    rc_angle=$?

    "${USHgfs}/radmon_verf_bcoef.sh"
    rc_bcoef=$?

    "${USHgfs}/radmon_verf_bcor.sh"
    rc_bcor=$?

    "${USHgfs}/radmon_verf_time.sh"
    rc_time=$?

    #--------------------------------------
    #  optionally run clean_tankdir script
    #
    if [[ ${CLEAN_TANKVERF:-0} -eq 1 ]]; then
       "${USHradmon}/clean_tankdir.sh" glb 60
       rc_clean_tankdir=$?
       echo "rc_clean_tankdir = ${rc_clean_tankdir}"
    fi

fi



#####################################################################
# Postprocessing

err=0
if [[ ${data_available} -ne 1 ]]; then
   err=1
elif [[ ${rc_angle} -ne 0 ]]; then
   err=${rc_angle}
elif [[ ${rc_bcoef} -ne 0 ]]; then
   err=${rc_bcoef}
elif [[ ${rc_bcor} -ne 0 ]]; then
   err=${rc_bcor}
elif [[ ${rc_time} -ne 0 ]]; then
   err=${rc_time}
fi

#####################################################################
# Restrict select sensors and satellites
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
rlist="saphir"
for rtype in ${rlist}; do
  if compgen -G "${TANKverf_rad}/"*"${rtype}"* > /dev/null; then
     ${CHGRP_CMD} "${TANKverf_rad}/"*"${rtype}"*
  fi
done

exit ${err}

