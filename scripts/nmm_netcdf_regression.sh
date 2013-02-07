#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

#. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-regtests/scripts/regression_var.sh
#. regression_var.sh

if [[ "$arch" = "Linux" ]]; then

   # Submit jobs using sub_zeus wrapper.

   /bin/sh sub_zeus -j $nmm_netcdf_updat_exp1 -t 0:30:00 -p 6/6/0 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_updat_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_updat_exp1}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh sub_zeus -j $nmm_netcdf_updat_exp2 -t 0:25:00 -p 6/8/0 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_updat_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_updat_exp2}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_updat_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh sub_zeus -j $nmm_netcdf_contrl_exp1 -t 0:30:00 -p 6/6/0 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_contrl_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_contrl_exp1}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_contrl_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh sub_zeus -j $nmm_netcdf_contrl_exp2 -t 0:25:00 -p 6/8/0 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_contrl_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_contrl_exp2}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_contrl_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh $scripts/regression_test.sh $nmm_netcdf_updat_exp1 $nmm_netcdf_updat_exp2 $nmm_netcdf_contrl_exp1 $nmm_netcdf_contrl_exp2 tmpreg_nmm_netcdf $nmm_netcdf_regression

   rm -f nmm_netcdf.out

   exit

elif [[ "$arch" = "AIX" ]]; then

   # Submit jobs using sub wrapper.

   /bin/sh sub -a RDAS-MTN -g $group -j $nmm_netcdf_updat_exp1 -q $queue -p 8/1/N -r 110/1 -t 0:10:00 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_updat_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_updat_exp1}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh sub -a RDAS-MTN -g $group -j $nmm_netcdf_updat_exp2 -q $queue -p 16/1/N -r 110/1 -t 0:10:00 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_updat_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_updat_exp2}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_updat_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh sub -a RDAS-MTN -g $group -j $nmm_netcdf_contrl_exp1 -q $queue -p 8/1/N -r 110/1 -t 0:10:00 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_contrl_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_contrl_exp1}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_contrl_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh sub -a RDAS-MTN -g $group -j $nmm_netcdf_contrl_exp2 -q $queue -p 16/1/N -r 110/1 -t 0:10:00 $scripts/nmm_netcdf.sh

   while [[ $(grep -c '+ rc=0' ${nmm_netcdf_contrl_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmm_netcdf_contrl_exp2}.out > return_code_nmm_netcdf.out
      if [ -s return_code_nmm_netcdf.out ]; then
         if [[ $(stat -c %s return_code_nmm_netcdf.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmm_netcdf.out) -ne 'rc=0' ]]; then
               echo $nmm_netcdf_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_netcdf.out)"."
               rm -f return_code_nmm_netcdf.out
               exit
            fi
         fi
      fi
      echo "Job "$nmm_netcdf_contrl_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmm_netcdf.out
   /bin/sh $scripts/regression_test.sh $nmm_netcdf_updat_exp1 $nmm_netcdf_updat_exp2 $nmm_netcdf_contrl_exp1 $nmm_netcdf_contrl_exp2 tmpreg_nmm_netcdf $nmm_netcdf_regression

   rm -f nmm_netcdf.out

   exit

fi
