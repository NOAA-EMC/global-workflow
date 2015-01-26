#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

#. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-regtests/scripts/regression_var.sh
#. regression_var.sh

if [[ "$machine" = "Zeus" ]]; then

   # Submit jobs using sub wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_zeus -j $rtma_updat_exp1 -t 0:15:00 -p 2/5/0 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_updat_exp1}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out

      /bin/sh sub_zeus -j $rtma_updat_exp2 -t 0:12:00 -p 4/5/0 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_updat_exp2}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out

      /bin/sh sub_zeus -j $rtma_contrl_exp1 -t 0:12:00 -p 2/5/0 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_contrl_exp1}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out

      /bin/sh sub_zeus -j $rtma_contrl_exp2 -t 0:12:00 -p 4/5/0 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_contrl_exp2}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out

      /bin/sh $scripts/regression_test.sh $rtma_updat_exp1 $rtma_updat_exp2 $rtma_contrl_exp1 $rtma_contrl_exp2 tmpreg_rtma $rtma_regression 10 10 2

      rm -f rtma.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_zeus -j $rtma_updat_exp1 -p 6/6/ -t 0:35:00 $scripts/rtma.sh

      rm -f rtma.out

      exit
   fi

   exit

elif [[ "$machine" = "WCOSS" ]]; then

   # Submit jobs using sub wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_wcoss -a RTMA-T2O -j $rtma_updat_exp1 -q $queue -p 10/1/ -r /1 -t 0:10:00 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_updat_exp1}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${rtma_updat_exp1}.out) -ne 1 ]]; do
         echo "Job "$rtma_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out
      /bin/sh sub_wcoss -a RTMA-T2O -j $rtma_updat_exp2 -q $queue -p 10/2/N -r /1 -t 0:10:00 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_updat_exp2}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${rtma_updat_exp2}.out) -ne 1 ]]; do
         echo "Job "$rtma_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out
      /bin/sh sub_wcoss -a RTMA-T2O -j $rtma_contrl_exp1 -q $queue -p 10/1/ -r /1 -t 0:10:00 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_contrl_exp1}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${rtma_contrl_exp1}.out) -ne 1 ]]; do
         echo "Job "$rtma_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out
      /bin/sh sub_wcoss -a RTMA-T2O -j $rtma_contrl_exp2 -q $queue -p 10/2/ -r /1 -t 0:10:00 $scripts/rtma.sh

      while [[ $(grep -c '+ rc=0' ${rtma_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${rtma_contrl_exp2}.out > return_code_rtma.out
         if [ -s return_code_rtma.out ]; then
            if [[ $(stat -c %s return_code_rtma.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_rtma.out) -ne 'rc=0' ]]; then
                  echo $rtma_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_rtma.out)"."
                  rm -f return_code_rtma.out
                  exit
               fi
            fi
         fi
         echo "Job "$rtma_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${rtma_contrl_exp2}.out) -ne 1 ]]; do
         echo "Job "$rtma_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_rtma.out
      /bin/sh $scripts/regression_test.sh $rtma_updat_exp1 $rtma_updat_exp2 $rtma_contrl_exp1 $rtma_contrl_exp2 tmpreg_rtma $rtma_regression 10 10 2

      rm -f rtma.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_wcoss -a RDAS-T2O -j $rtma_updat_exp1 -q $queue -p 10/1/ -t 0:35:00 $scripts/rtma.sh

      rm -f rtma.out

      exit
   fi

   exit

fi
