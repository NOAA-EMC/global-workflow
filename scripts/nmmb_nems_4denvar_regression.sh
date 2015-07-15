#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

#. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-regtests/scripts/regression_var.sh
#. regression_var.sh

if [ "$machine" = "Zeus" -o "$machine" = "Theia" ]; then

   # Submit jobs using sub wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_zeus -j $nmmb_nems_4denvar_updat_exp1 -t 0:30:00 -p 7/10/0 -r /1 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_updat_exp1}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out

      /bin/sh sub_zeus -j $nmmb_nems_4denvar_updat_exp2 -t 0:25:00 -p 9/10/0 -r /1 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_updat_exp2}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out

      /bin/sh sub_zeus -j $nmmb_nems_4denvar_contrl_exp1 -t 0:30:00 -p 7/10/0 -r /1 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_contrl_exp1}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out

      /bin/sh sub_zeus -j $nmmb_nems_4denvar_contrl_exp2 -t 0:25:00 -p 9/10/0 -r /1 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_contrl_exp2}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out

      /bin/sh $scripts/regression_test.sh $nmmb_nems_4denvar_updat_exp1 $nmmb_nems_4denvar_updat_exp2 $nmmb_nems_4denvar_contrl_exp1 $nmmb_nems_4denvar_contrl_exp2 tmpreg_nems_nmmb_4denvar $nems_nmmb_4denvar_regression 8 10 8

      rm -f nmmb_nems_4denvar.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_zeus -j $nmmb_nems_4denvar_updat_exp1 -p 7/10/ -t 1:00:00 $scripts/nmmb_nems_4denvar.sh

      rm -f nmmb_nems_4denvar.out

      exit
   fi

   exit

elif [[ "$machine" = "WCOSS" ]]; then

   # Submit jobs using sub wrapper.
   if [ "$debug" = ".false." ]; then
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmmb_nems_4denvar_updat_exp1 -q $queue -p 7/10/ -r /1 -t 0:25:00 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_updat_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_updat_exp1}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_updat_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmmb_nems_4denvar_updat_exp1}.out) -ne 1 ]]; do
         echo "Job "$nmmb_nems_4denvar_updat_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmmb_nems_4denvar_updat_exp2 -q $queue -p 9/10/ -r /2 -t 0:20:00 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_updat_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_updat_exp2}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_updat_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmmb_nems_4denvar_updat_exp2}.out) -ne 1 ]]; do
         echo "Job "$nmmb_nems_4denvar_updat_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmmb_nems_4denvar_contrl_exp1 -q $queue -p 7/10/ -r /1 -t 0:25:00 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_contrl_exp1}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_contrl_exp1}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_contrl_exp1" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmmb_nems_4denvar_contrl_exp1}.out) -ne 1 ]]; do
         echo "Job "$nmmb_nems_4denvar_contrl_exp1" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmmb_nems_4denvar_contrl_exp2 -q $queue -p 9/10/ -r /2 -t 0:20:00 $scripts/nmmb_nems_4denvar.sh

      while [[ $(grep -c '+ rc=0' ${nmmb_nems_4denvar_contrl_exp2}.out) -ne 1 ]]; do
         grep '+ rc=' ${nmmb_nems_4denvar_contrl_exp2}.out > return_code_nmmb_nems_4denvar.out
         if [ -s return_code_nmmb_nems_4denvar.out ]; then
            if [[ $(stat -c %s return_code_nmmb_nems_4denvar.out) -ne '0' ]]; then
               if [[ $(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out) -ne 'rc=0' ]]; then
                  echo $nmmb_nems_4denvar_contrl_exp2" job has failed with return code of "$(awk '{ print $2 }' return_code_nmmb_nems_4denvar.out)"."
                  rm -f return_code_nmmb_nems_4denvar.out
                  exit
               fi
            fi
         fi
         echo "Job "$nmmb_nems_4denvar_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      while [[ $(grep -c 'Resource usage summary:' ${nmmb_nems_4denvar_contrl_exp2}.out) -ne 1 ]]; do
         echo "Job "$nmmb_nems_4denvar_contrl_exp2" is not complete yet.  Will recheck in a minute."
         sleep 60
      done

      rm -f return_code_nmmb_nems_4denvar.out
      /bin/sh $scripts/regression_test.sh $nmmb_nems_4denvar_updat_exp1 $nmmb_nems_4denvar_updat_exp2 $nmmb_nems_4denvar_contrl_exp1 $nmmb_nems_4denvar_contrl_exp2 tmpreg_nems_nmmb_4denvar $nems_nmmb_4denvar_regression 8 10 8

      rm -f nmmb_nems_4denvar.out

      exit

   elif [ "$debug" = .true. ]; then
      /bin/sh sub_wcoss -a RDAS-T2O -j $nmmb_nems_4denvar_updat_exp1 -q $queue -p 7/10/ -t 1:00:00 $scripts/nmmb_nems_4denvar.sh

      rm -f nmmb_nems_4denvar.out

      exit
   fi

   exit

fi
