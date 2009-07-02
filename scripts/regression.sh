#!/bin/sh

#@ job_name=regression_driver
#@ error=regression_driver.e$(jobid)
#@ job_type=serial
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ class=dev
#@ group=dev
#@ wall_clock_limit = 00:10:00
#@ account_no = GDAS-MTN
#@ notification=error
#@ queue

. regression_var.sh

rm -rf $regression_vfydir

if [[ $control == true ]]; then

   rm -rf $noscrub/tmpreg_${rtma}
   rm -rf $noscrub/tmp${global}
   rm -rf $noscrub/tmpreg_${arw_binary}
   rm -rf $noscrub/tmpreg_${arw_netcdf}
   rm -rf $noscrub/tmpreg_${nmm_binary}
   rm -rf $noscrub/tmpreg_${nmm_netcdf}

   list="global_T62 RTMA nmm_binary nmm_netcdf arw_binary arw_netcdf"
   for configuration in $list; do
       llsubmit regression_$configuration.sh
   done

elif [[ $control == false ]]; then
     list="global_T62_nc RTMA_nc nmm_binary_nc nmm_netcdf_nc arw_binary_nc arw_netcdf_nc"
     for configuration in $list; do
         llsubmit regression_$configuration.sh
     done

fi

rm -f regression_driver.e*

exit
