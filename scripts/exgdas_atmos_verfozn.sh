#! /usr/bin/env bash

################################################################################
# exgdas_atmos_verfozn.sh
#
# This script runs the data extract/validation portion of the Ozone Monitor
# (OznMon) DA package.
#
################################################################################
export err=0

data_available=0

if [[ -s ${oznstat} ]]; then
   data_available=1

   #------------------------------------------------------------------
   #  Copy data files file to local data directory.
   #  Untar oznstat file.
   #------------------------------------------------------------------

   cpreq "${oznstat}" "./oznstat.${PDY}${cyc}"

   tar -xvf "oznstat.${PDY}${cyc}"
   rm -f "oznstat.${PDY}${cyc}"

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

   "${USHgfs}/ozn_xtrct.sh" && true
   export err=$?
   if [[ ${err} -ne 0 ]]; then
     err_exit "ozn_xtrct.sh failed!"
   fi

else
   # oznstat file not found
   export err=1
   err_exit "${oznstat} does not exist!"
fi
exit 0
