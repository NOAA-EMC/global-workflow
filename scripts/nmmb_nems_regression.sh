#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

#. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-regtests/scripts/regression_var.sh
#. regression_var.sh

if [[ "$arch" = "Linux" ]]; then

   # Submit jobs using sub wrapper.

   /bin/sh sub_zeus -j $nmmb_nems_updat_exp1 -t 0:15:00 -p 6/6/0 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_updat_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_updat_exp1}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_updat_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh sub_zeus -j $nmmb_nems_updat_exp2 -t 0:12:00 -p 6/8/0 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_updat_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_updat_exp2}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_updat_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh sub_zeus -j $nmmb_nems_contrl_exp1 -t 0:12:00 -p 6/8/0 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_contrl_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_contrl_exp1}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_contrl_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh sub_zeus -j $nmmb_nems_contrl_exp2 -t 0:12:00 -p 6/8/0 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_contrl_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_contrl_exp2}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_contrl_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh $scripts/regression_test.sh $nmmb_nems_updat_exp1 $nmmb_nems_updat_exp2 $nmmb_nems_contrl_exp1 $nmmb_nems_contrl_exp2 tmpreg_nems_nmmb $nems_nmmb_regression

   rm -f nmmb_nems.out

   exit

elif [[ "$arch" = "AIX" ]]; then

   # Submit jobs using sub wrapper.

   /bin/sh sub -a RDAS-MTN -g $group -j $nmmb_nems_updat_exp1 -q $queue -p 32/1/N -r 110/1 -t 0:15:00 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_updat_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_updat_exp1}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_updat_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh sub -a RDAS-MTN -g $group -j $nmmb_nems_updat_exp2 -q $queue -p 32/2/N -r 110/2 -t 0:10:00 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_updat_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_updat_exp2}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_updat_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh sub -a RDAS-MTN -g $group -j $nmmb_nems_contrl_exp1 -q $queue -p 32/1/N -r 110/1 -t 0:15:00 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_contrl_exp1}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_contrl_exp1}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_contrl_exp1" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh sub -a RDAS-MTN -g $group -j $nmmb_nems_contrl_exp2 -q $queue -p 32/2/N -r 110/2 -t 0:10:00 $scripts/nmmb_nems.sh

   while [[ $(grep -c '+ rc=0' ${nmmb_nems_contrl_exp2}.out) -ne 1 ]]; do
      grep '+ rc=' ${nmmb_nems_contrl_exp2}.out > return_code_nmmb_nems.out
      if [ -s return_code_nmmb_nems.out ]; then
         if [[ $(stat -c %s return_code_nmmb_nems.out) -ne '0' ]]; then
            if [[ $(awk '{ print $2 }' return_code_nmmb_nems.out) -ne 'rc=0' ]]; then
               echo $nmmb_nems_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems.out)"."
               rm -f return_code_nmmb_nems.out
               exit
            fi
         fi
      fi
      echo "Job "$nmmb_nems_contrl_exp2" is not complete yet.  Will recheck in a minute."
      sleep 60
   done

   rm -f return_code_nmmb_nems.out
   /bin/sh $scripts/regression_test.sh $nmmb_nems_updat_exp1 $nmmb_nems_updat_exp2 $nmmb_nems_contrl_exp1 $nmmb_nems_contrl_exp2 tmpreg_nems_nmmb $nems_nmmb_regression

   rm -f nmmb_nems.out

   exit

fi
