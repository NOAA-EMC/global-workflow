#! /usr/bin/env bash

# Scripts used
SFS_6HRLY_VARSSH=${SFS_6HRLY_VARSSH:-"${HOMEgfs}/sorc/gfs_utils.fd/ush/sfs_6hrly_vars.sh"}
SFS_ATMOS_DAILYSH=${SFS_ATMOS_DAILYSH:-"${HOMEgfs}/sorc/gfs_utils.fd/ush/sfs_atmos_daily.sh"}
SFS_ATMOS_MONTHLYSH=${SFS_ATMOS_MONTHLYSH:-"${HOMEgfs}/sorc/gfs_utils.fd/ush/sfs_atmos_monthly.sh"}

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

##############################################
# End JOB SPECIFIC work
##############################################

exit 0
