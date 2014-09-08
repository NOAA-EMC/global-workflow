#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

#. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-regtests/scripts/regression_var.sh
#. regression_var.sh

if [[ "$machine" = "Zeus" ]]; then

   # Submit jobs using sub_zeus wrapper.

   /bin/sh sub_zeus -j $hwrf_nmm_d2_updat_exp1 -t 0:20:00 -p 6/6/0 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_updat_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_updat_exp1}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_updat_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out

   /bin/sh sub_zeus -j $hwrf_nmm_d2_updat_exp2 -t 0:15:00 -p 8/8/0 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_updat_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_updat_exp2}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_updat_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out

   /bin/sh sub_zeus -j $hwrf_nmm_d2_contrl_exp1 -t 0:20:00 -p 6/6/0 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_contrl_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_contrl_exp1}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_contrl_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out

   /bin/sh sub_zeus -j $hwrf_nmm_d2_contrl_exp2 -t 0:15:00 -p 8/8/0 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_contrl_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_contrl_exp2}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_contrl_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out

   /bin/sh $scripts/regression_test.sh $hwrf_nmm_d2_updat_exp1 $hwrf_nmm_d2_updat_exp2 $hwrf_nmm_d2_contrl_exp1 $hwrf_nmm_d2_contrl_exp2 tmpreg_hwrf_nmm_d2 $hwrf_nmm_d2_regression 5 10 2

   rm -f hwrf_nmm_d2.out

   exit

elif [[ "$machine" = "WCOSS" ]]; then

   # Submit jobs using sub wrapper.

   /bin/sh sub_wcoss -a RDAS-MTN -j $hwrf_nmm_d2_updat_exp1 -q $queue -p 6/6/ -r /1 -t 0:20:00 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_updat_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_updat_exp1}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_updat_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   while [[ $(grep -c 'Resource usage summary:' ${hwrf_nmm_d2_updat_exp1}.out) -ne 1 ]]; do
      echo "Job "$hwrf_nmm_d2_updat_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out
   /bin/sh sub_wcoss -a RDAS-MTN -j $hwrf_nmm_d2_updat_exp2 -q $queue -p 8/8/ -r /1 -t 0:15:00 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_updat_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_updat_exp2}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_updat_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   while [[ $(grep -c 'Resource usage summary:' ${hwrf_nmm_d2_updat_exp2}.out) -ne 1 ]]; do
      echo "Job "$hwrf_nmm_d2_updat_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out
   /bin/sh sub_wcoss -a RDAS-MTN -j $hwrf_nmm_d2_contrl_exp1 -q $queue -p 6/6/ -r 110/ -t 0:20:00 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_contrl_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_contrl_exp1}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_contrl_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   while [[ $(grep -c 'Resource usage summary:' ${hwrf_nmm_d2_contrl_exp1}.out) -ne 1 ]]; do
      echo "Job "$hwrf_nmm_d2_contrl_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out
   /bin/sh sub_wcoss -a RDAS-MTN -j $hwrf_nmm_d2_contrl_exp2 -q $queue -p 8/8/ -r /1 -t 0:20:00 $scripts/hwrf_nmm_d2.sh

   while [[ $(grep -c '+ rc=0' ${hwrf_nmm_d2_contrl_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${hwrf_nmm_d2_contrl_exp2}.out > return_code_hwrf_nmm_d2.out
      if [ -s return_code_hwrf_nmm_d2.out ]; then
         if [[ $(stat -c %s return_code_hwrf_nmm_d2.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_hwrf_nmm_d2.out) -ne 'rc=0' ]]; then
               echo $hwrf_nmm_d2_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_hwrf_nmm_d2.out)"."
               rm -f return_code_hwrf_nmm_d2.out
               exit
            fi
         fi
      fi
      echo "Job "$hwrf_nmm_d2_contrl_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   while [[ $(grep -c 'Resource usage summary:' ${hwrf_nmm_d2_contrl_exp2}.out) -ne 1 ]]; do
      echo "Job "$hwrf_nmm_d2_contrl_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_hwrf_nmm_d2.out
   /bin/sh $scripts/regression_test.sh $hwrf_nmm_d2_updat_exp1 $hwrf_nmm_d2_updat_exp2 $hwrf_nmm_d2_contrl_exp1 $hwrf_nmm_d2_contrl_exp2 tmpreg_hwrf_nmm_d2 $hwrf_nmm_d2_regression 5 10 2

   rm -f hwrf_nmm_d2.out

   exit

fi
