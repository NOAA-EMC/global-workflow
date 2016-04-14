#!/bin/sh

set -x

# regression test to launch
export regtest=$1

# source the necessary files to setup
. $(awk '{ print $1 }' regression_var.out)
. $scripts/regression_param.sh $regtest

if [ "$debug" = ".false." ]; then

   # Launch the individual control and update runs, one-after-another
   for jn in `seq 1 4`; do

      rm -f ${job[$jn]}.out

      /bin/sh $sub_cmd -q $queue -j ${job[$jn]} -t ${topts[$jn]} -p ${popts[$jn]} -r ${ropts[$jn]} $scripts/${regtest}.sh

      $scripts/regression_wait.sh ${job[$jn]} ${rcname} $check_resource
      rc=$?
      if [ $rc -ne 0 ]; then exit; fi

   done

   # When all done, test the results of the regression test
   if [ $regtest = 'global_enkf_T62' ]; then
      /bin/sh $scripts/regression_test_enkf.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} ${tmpregdir} ${result} ${scaling[1]} ${scaling[2]} ${scaling[3]}
   else
      /bin/sh $scripts/regression_test.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} ${tmpregdir} ${result} ${scaling[1]} ${scaling[2]} ${scaling[3]}
   fi

else

   /bin/sh $sub_cmd -q $queue -j ${job[1]} -p ${popts[1]} -t ${topts[1]} $scripts/${regtest}.sh

fi

# Clean-up
rm -f ${regtest}.out

exit
