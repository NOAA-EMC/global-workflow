#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

################################################################################
# exgdas_atmos_verfozn.sh
#
# This script runs the data extract/validation portion of the Ozone Monitor
# (OznMon) DA package.
#
################################################################################
err=0

data_available=0

if [[ -s ${oznstat} ]]; then
   data_available=1

   #------------------------------------------------------------------
   #  Copy data files file to local data directory.
   #  Untar oznstat file.
   #------------------------------------------------------------------

   ${NCP} "${oznstat}" "./oznstat.${PDY}${cyc}"

   tar -xvf "oznstat.${PDY}${cyc}"
   rm "oznstat.${PDY}${cyc}"

   netcdf=0
   count=$(ls diag* | grep ".nc4" | wc -l)
   if [ "${count}" -gt 0 ] ; then
      netcdf=1
      for filenc4 in $(ls diag*nc4.gz); do
         file=$(echo "${filenc4}" | cut -d'.' -f1-2).gz
         mv "${filenc4}" "${file}"
      done
   fi

   export OZNMON_NETCDF=${netcdf}

   "${USHgfs}/ozn_xtrct.sh"
   err=$?

else
   # oznstat file not found
   err=1
fi

exit ${err}

