#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

job[1]=$nmm_netcdf_updat_exp1
job[2]=$nmm_netcdf_updat_exp2
job[3]=$nmm_netcdf_contrl_exp1
job[4]=$nmm_netcdf_contrl_exp2

if [[ "$machine" = "Theia" ]]; then

   topts[1]="0:15:00" ; popts[1]="4/2/"  ; ropts[1]="/1"
   topts[2]="0:15:00" ; popts[2]="4/4/"  ; ropts[2]="/1"

   sub_cmd="sub_zeus"

elif [[ "$machine" = "WCOSS" ]]; then

   topts[1]="0:15:00" ; popts[1]="8/1/"  ; ropts[1]="/1"
   topts[2]="0:15:00" ; popts[2]="16/1/" ; ropts[2]="/2"

   sub_cmd="sub_wcoss -a RDAS-T2O -q $queue"

fi

topts[3]=${topts[1]} ; popts[3]=${popts[1]} ; ropts[3]=${ropts[1]}
topts[4]=${topts[2]} ; popts[4]=${popts[2]} ; ropts[4]=${ropts[2]}

if [ "$debug" = ".false." ]; then

   for jn in `seq 1 4`; do

      rm -f ${job[$jn]}.out

      /bin/sh $sub_cmd -j ${job[$jn]} -t ${topts[$jn]} -p ${popts[$jn]} -r ${ropts[$jn]} $scripts/nmm_netcdf.sh

      $scripts/regression_wait.sh ${job[$jn]} return_code_nmm_netcdf.out check_resource

   done

   /bin/sh $scripts/regression_test.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} tmpreg_nmm_netcdf $nmm_netcdf_regression 5 10 2

else
  
   if [ "$machine" = "Theia" ]; then
      /bin/sh $sub_cmd -j ${job[1]} -p ${popts[1]} -t 0:30:00 $scripts/nmm_netcdf.sh
   elif [ "$machine" = "WCOSS" ]; then
      /bin/sh $sub_cmd -j ${job[1]} -p ${popts[1]} -t 0:30:00 $scripts/nmm_netcdf.sh
   fi

fi

rm -f nmm_netcdf.out

exit
