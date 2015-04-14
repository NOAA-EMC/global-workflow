#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

#. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-regtests/scripts/regression_var.sh
#. regression_var.sh

if [ "$machine" = "Zeus" -o "$machine" = "Theia" ]; then

   # Submit jobs using sub_zeus wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_zeus -j $nmm_binary_updat_exp1 -t 0:30:00 -p 6/6/0 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_updat_exp1}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out

      /bin/sh sub_zeus -j $nmm_binary_updat_exp2 -t 0:25:00 -p 8/8/0 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_updat_exp2}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out

      /bin/sh sub_zeus -j $nmm_binary_contrl_exp1 -t 0:30:00 -p 6/6/0 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_contrl_exp1}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out

      /bin/sh sub_zeus -j $nmm_binary_contrl_exp2 -t 0:25:00 -p 8/8/0 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_contrl_exp2}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out

      /bin/sh $scripts/regression_test.sh $nmm_binary_updat_exp1 $nmm_binary_updat_exp2 $nmm_binary_contrl_exp1 $nmm_binary_contrl_exp2 tmpreg_nmm_binary $nmm_binary_regression 8 10 8

      rm -f nmm_binary.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_zeus -j $nmm_binary_updat_exp1 -p 6/6/ -t 0:35:00 $scripts/nmm_binary.sh

      rm -f nmm_binary.out

      exit
   fi

   exit

elif [[ "$machine" = "WCOSS" ]]; then

   # Submit jobs using sub wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmm_binary_updat_exp1 -q $queue -p 7/12/ -r /1 -t 0:30:00 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_updat_exp1}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmm_binary_updat_exp1}.out) -ne 1 ]]; do
         echo "Job "$nmm_binary_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmm_binary_updat_exp2 -q $queue -p 9/12/ -r /2 -t 0:25:00 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_updat_exp2}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmm_binary_updat_exp2}.out) -ne 1 ]]; do
         echo "Job "$nmm_binary_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmm_binary_contrl_exp1 -q $queue -p 7/12/ -r /1 -t 0:30:00 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_contrl_exp1}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmm_binary_contrl_exp1}.out) -ne 1 ]]; do
         echo "Job "$nmm_binary_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmm_binary_contrl_exp2 -q $queue -p 9/12/ -r /2 -t 0:25:00 $scripts/nmm_binary.sh

      while [[ $(grep -c '+ rc=0' ${nmm_binary_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmm_binary_contrl_exp2}.out > return_code_nmm_binary.out
         if [ -s return_code_nmm_binary.out ]; then
            if [[ $(stat -c %s return_code_nmm_binary.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmm_binary.out) -ne 'rc=0' ]]; then
                  echo $nmm_binary_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmm_binary.out)"."
                  rm -f return_code_nmm_binary.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmm_binary_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmm_binary_contrl_exp2}.out) -ne 1 ]]; do
         echo "Job "$nmm_binary_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmm_binary.out
      /bin/sh $scripts/regression_test.sh $nmm_binary_updat_exp1 $nmm_binary_updat_exp2 $nmm_binary_contrl_exp1 $nmm_binary_contrl_exp2 tmpreg_nmm_binary $nmm_binary_regression 8 10 8

      rm -f nmm_binary.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmm_binary_updat_exp1 -q $queue -p 7/12/ -t 0:35:00 $scripts/nmm_binary.sh

      rm -f nmm_binary.out

      exit
   fi

   exit

fi
