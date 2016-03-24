#!/bin/sh

set -x

. $(awk '{ print $1 }' regression_var.out)

export regtest=$1

. $scripts/regression_param.sh $regtest

if [ "$debug" = ".false." ]; then

   for jn in `seq 1 4`; do

      rm -f ${job[$jn]}.out

      /bin/sh $sub_cmd -j ${job[$jn]} -t ${topts[$jn]} -p ${popts[$jn]} -r ${ropts[$jn]} $scripts/${regtest}.sh

      $scripts/regression_wait.sh ${job[$jn]} ${rcname} $check_resource

   done

   if [ $regtest = 'global_enkf_T62' ]; then
      /bin/sh $scripts/regression_test_enkf.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} ${tmpregdir} ${result} ${scaling[1]} ${scaling[2]} ${scaling[3]}
   else
      /bin/sh $scripts/regression_test.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} ${tmpregdir} ${result} ${scaling[1]} ${scaling[2]} ${scaling[3]}
   fi

else
  
   /bin/sh $sub_cmd -j ${job[1]} -p ${popts[1]} -t ${topts[1]} $scripts/${regtest}.sh

fi

rm -f ${regtest}.out

exit
