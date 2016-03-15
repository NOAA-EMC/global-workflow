#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

job[1]=$arw_binary_updat_exp1
job[2]=$arw_binary_updat_exp2
job[3]=$arw_binary_contrl_exp1
job[4]=$arw_binary_contrl_exp2

if [[ "$machine" = "Theia" ]]; then

   topts[1]="0:15:00" ; popts[1]="4/4/"  ; ropts[1]="/1"
   topts[2]="0:15:00" ; popts[2]="6/6/"  ; ropts[2]="/1"

   sub_cmd="sub_zeus -q $queue"

elif [[ "$machine" = "WCOSS" ]]; then

   topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="/1"
   topts[2]="0:15:00" ; popts[2]="16/2/" ; ropts[2]="/1"

   sub_cmd="sub_wcoss -a RDAS-T2O -q $queue"

fi

topts[3]=${topts[1]} ; popts[3]=${popts[1]} ; ropts[3]=${ropts[1]}
topts[4]=${topts[2]} ; popts[4]=${popts[2]} ; ropts[4]=${ropts[2]}

if [ "$debug" = ".false." ]; then

   for jn in `seq 1 4`; do

      rm -f ${job[$jn]}.out

      /bin/sh $sub_cmd -j ${job[$jn]} -t ${topts[$jn]} -p ${popts[$jn]} -r ${ropts[$jn]} $scripts/arw_binary.sh

      $scripts/regression_wait.sh ${job[$jn]} return_code_arw_binary.out $check_resource

   done

   /bin/sh $scripts/regression_test.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} tmpreg_arw_binary $arw_binary_regression 4 8 4

else
  
   if [ "$machine" = "Theia" ]; then
      /bin/sh $sub_cmd -j ${job[1]} -p ${popts[1]} -t 0:30:00 $scripts/arw_binary.sh
   elif [ "$machine" = "WCOSS" ]; then
      /bin/sh $sub_cmd -j ${job[1]} -p ${popts[1]} -t 0:30:00 $scripts/arw_binary.sh
   fi

fi

rm -f arw_binary.out

exit
