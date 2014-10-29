#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

#. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-regtests/scripts/regression_var.sh
#. regression_var.sh

if [[ "$machine" = "Zeus" ]]; then

   # Submit jobs using sub_zeus wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_zeus -j $arw_netcdf_updat_exp1 -t 0:15:00 -p 4/4/0 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_updat_exp1}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out

      /bin/sh sub_zeus -j $arw_netcdf_updat_exp2 -t 0:12:00 -p 6/6/0 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_updat_exp2}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out

      /bin/sh sub_zeus -j $arw_netcdf_contrl_exp1 -t 0:15:00 -p 4/4/0 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_contrl_exp1}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out

      /bin/sh sub_zeus -j $arw_netcdf_contrl_exp2 -t 0:12:00 -p 6/6/0 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_contrl_exp2}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out

      /bin/sh $scripts/regression_test.sh $arw_netcdf_updat_exp1 $arw_netcdf_updat_exp2 $arw_netcdf_contrl_exp1 $arw_netcdf_contrl_exp2 tmpreg_arw_netcdf $arw_netcdf_regression 4 10 4

      rm -f arw_netcdf.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_zeus -j $arw_netcdf_updat_exp1 -p 6/6/ -t 0:35:00 $scripts/arw_netcdf.sh

      rm -f arw_netcdf.out

      exit
   fi

   exit

elif [[ "$machine" = "WCOSS" ]]; then

   # Submit jobs using sub wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_wcoss -a RDAS-T2O -j $arw_netcdf_updat_exp1 -q $queue -p 16/1/ -r /1 -t 0:10:00 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_updat_exp1}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${arw_netcdf_updat_exp1}.out) -ne 1 ]]; do
         echo "Job "$arw_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $arw_netcdf_updat_exp2 -q $queue -p 16/2/ -r /1 -t 0:10:00 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_updat_exp2}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${arw_netcdf_updat_exp2}.out) -ne 1 ]]; do
         echo "Job "$arw_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $arw_netcdf_contrl_exp1 -q $queue -p 16/1/ -r /1 -t 0:10:00 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_contrl_exp1}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${arw_netcdf_contrl_exp1}.out) -ne 1 ]]; do
         echo "Job "$arw_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $arw_netcdf_contrl_exp2 -q $queue -p 16/2/ -r /1 -t 0:10:00 $scripts/arw_netcdf.sh

      while [[ $(grep -c '+ rc=0' ${arw_netcdf_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${arw_netcdf_contrl_exp2}.out > return_code_arw_netcdf.out
         if [ -s return_code_arw_netcdf.out ]; then
            if [[ $(stat -c %s return_code_arw_netcdf.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_arw_netcdf.out) -ne 'rc=0' ]]; then
                  echo $arw_netcdf_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_arw_netcdf.out)"."
                  rm -f return_code_arw_netcdf.out
                  exit
               fi
            fi
         fi
         echo "Job "$arw_netcdf_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${arw_netcdf_contrl_exp2}.out) -ne 1 ]]; do
         echo "Job "$arw_netcdf_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_arw_netcdf.out
      /bin/sh $scripts/regression_test.sh $arw_netcdf_updat_exp1 $arw_netcdf_updat_exp2 $arw_netcdf_contrl_exp1 $arw_netcdf_contrl_exp2 tmpreg_arw_netcdf $arw_netcdf_regression 4 8 4

      rm -f arw_netcdf.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_wcoss -a RDAS-T2O -j $arw_netcdf_updat_exp1 -q $queue -p 16/1/ -t 0:15:00 $scripts/arw_netcdf.sh

      rm -f arw_netcdf.out

      exit
   fi

   exit

fi
