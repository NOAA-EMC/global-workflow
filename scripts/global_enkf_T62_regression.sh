#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

job[1]=$global_enkf_T62_updat_exp1
job[2]=$global_enkf_T62_updat_exp2
job[3]=$global_enkf_T62_contrl_exp1
job[4]=$global_enkf_T62_contrl_exp2

if [[ "$machine" = "Theia" ]]; then

   topts[1]="0:25:00" ; popts[1]="12/3/" ; ropts[1]="/1"
   topts[2]="0:25:00" ; popts[2]="12/5/" ; ropts[2]="/2"

   sub_cmd="sub_zeus -q $queue"

elif [[ "$machine" = "WCOSS" ]]; then

   topts[1]="0:25:00" ; popts[1]="16/2/" ; ropts[1]="/1"
   topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"

   sub_cmd="sub_wcoss -a GDAS-T2O -q $queue"

fi

topts[3]=${topts[1]} ; popts[3]=${popts[1]} ; ropts[3]=${ropts[1]}
topts[4]=${topts[2]} ; popts[4]=${popts[2]} ; ropts[4]=${ropts[2]}

if [ "$debug" = ".false." ]; then

   for jn in `seq 1 4`; do

      rm -f ${job[$jn]}.out

      /bin/sh $sub_cmd -j ${job[$jn]} -t ${topts[$jn]} -p ${popts[$jn]} -r ${ropts[$jn]} $scripts/global_enkf_T62.sh

      $scripts/regression_wait.sh ${job[$jn]} return_code_global_enkf.out $check_resource

   done

   /bin/sh $scripts/regression_test_enkf.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} tmp62_enkf $global_enkf_regression 10 8 2

else
  
   if [ "$machine" = "Theia" ]; then
      /bin/sh $sub_cmd -j ${job[1]} -p ${popts[1]} -t 1:00:00 $scripts/global_enkf_T62.sh
   elif [ "$machine" = "WCOSS" ]; then
      /bin/sh $sub_cmd -j ${job[1]} -p ${popts[1]} -t 1:00:00 $scripts/global_enkf_T62.sh
   fi

fi

rm -f global_enkf_T62.out

exit
