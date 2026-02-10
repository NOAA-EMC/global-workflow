#! /usr/bin/env bash

# Scripts used
SFS_6HRLY_VARSSH=${SFS_6HRLY_VARSSH:-"${USHgfs}/sfs_6hrly_vars.sh"}
SFS_ATMOS_DAILYSH=${SFS_ATMOS_DAILYSH:-"${USHgfs}/sfs_atmos_daily.sh"}
SFS_ATMOS_MONTHLYSH=${SFS_ATMOS_MONTHLYSH:-"${USHgfs}/sfs_atmos_monthly.sh"}

# Check if GMERGE is set; if not, print error and exit with status 1
if [[ -z "${GMERGE:-}" ]]; then
    echo "Error: GMERGE is not defined. Exiting script." >&2
    exit 1
fi

# Check if WGRIB2 is set; if not, print error and exit with status 1
if [[ -z "${WGRIB2:-}" ]]; then
    echo "Error: WGRIB2 is not defined. Exiting script." >&2
    exit 1
fi

##############################################
# Begin JOB SPECIFIC work
##############################################

if [[ "${RUN}" == sfs ]]; then
    #GENERATE 6-HOURLY GRIB2 FILES FROM SELECTED SFS VARIABLES.
      "${SFS_6HRLY_VARSSH}" && true
       export err=$?
       if [[ ${err} -ne 0 ]]; then
          echo "FATAL ERROR: Failed to generate 6-hourly grib2 files"
          exit "${err}"
       fi

    #GENERATE DAILY MEAN GRIB2 FILES FROM SFS MASTER 6-HOURLY DATA FILES.
       "${SFS_ATMOS_DAILYSH}" && true
       export err=$?
       if [[ ${err} -ne 0 ]]; then
         echo "FATAL ERROR: Failed to generate daily mean grib2 files"
         exit "${err}"
       fi

    #GENERATE MONTHLY MEAN GRIB2 FILES FROM SFS MASTER 6-HOURLY DATA FILES.
      "${SFS_ATMOS_MONTHLYSH}" && true
       export err=$?
       if [[ ${err} -ne 0 ]]; then
          echo "FATAL ERROR: Failed to generate monthly mean grib2 files"
          exit "${err}"
       fi
 fi

# Remove the original atmos master files if all atmos products are successfully generated.
status=$?
if [[ ${status} -eq 0 ]]; then
   echo "All atmospheric products are successfully generated!"
   rm -f "${COMIN_ATMOS_MASTER}/sfs"*"grib2"
fi

##############################################
# End JOB SPECIFIC work
##############################################

exit 0
