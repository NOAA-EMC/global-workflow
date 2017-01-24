#!/bin/sh

set -x

# regression test to launch
export regtest=$1

# source the necessary files to setup
. $(awk '{ print $1, $2, $3, $4, $5, $6, $7, $8, $9 }' regression_var.out)
export scripts=${scripts_updat:-$scripts}
. $scripts/regression_param.sh $regtest

# Launch the individual control and update runs, one-after-another
for jn in `seq 1 4`; do

   if [ $jn -le 2 ]; then
      export scripts=${scripts_updat:-$scripts}
      export fixgsi=${fixgsi_updat:-$fixgsi}
   else
      export scripts=${scripts_contrl:-$scripts}
      export fixgsi=${fixgsi_contrl:-$fixgsi}
   fi
   rm -f ${job[$jn]}.out

   /bin/sh $sub_cmd -q $queue -j ${job[$jn]} -t ${topts[$jn]} -p ${popts[$jn]} -r ${ropts[$jn]} $scripts/${regtest}.sh

   if [ $debug == ".true." ]; then break; fi
      $scripts/regression_wait.sh ${job[$jn]} ${rcname} $check_resource
      rc=$?
      if [ $rc -ne 0 ]; then
         rm -f ${rcname}
         exit 1
      fi
   done
# When all done, test the results of the regression test
if [ "$debug" = ".false." ]; then

   export scripts=${scripts_updat:-$scripts}

   if [ $regtest = 'global_enkf_T62' ]; then
      /bin/sh $scripts/regression_test_enkf.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} ${tmpregdir} ${result} ${scaling[1]} ${scaling[2]} ${scaling[3]}
   else
      /bin/sh $scripts/regression_test.sh ${job[1]} ${job[2]} ${job[3]} ${job[4]} ${tmpregdir} ${result} ${scaling[1]} ${scaling[2]} ${scaling[3]}
   fi
   rc=$?
   if [ $rc -ne 0 ]; then
      exit 1
   fi
fi

# Clean-up
rm -f ${regtest}.out

exit
