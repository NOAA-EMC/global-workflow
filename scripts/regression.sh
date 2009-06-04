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

list="global_T62 RTMA nmm_binary nmm_netcdf arw_binary arw_netcdf"
for configuration in $list; do
   llsubmit regression_$configuration.sh
done

exit
