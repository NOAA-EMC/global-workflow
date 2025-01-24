#!/bin/sh

set -x

# regression test to launch
export regtest=$1

# option to set local values with a config file
if [ -d "$config_path" ]; then
    source $config_path/local_vars.sh
fi

# source the necessary files to setup
if [ "$#" -eq 2 ]; then
  export regdir=$2
  . $(awk '{ print $1, $2, $3, $4, $5, $6, $7, $8 }' $regdir/regression_var.out)
else
  export regdir=$(pwd)
  . $(awk '{ print $1, $2, $3, $4, $5, $6, $7, $8 }' regression_var.out)
fi

export scripts=${scripts_updat:-$scripts}
. $scripts/regression_param.sh $regtest

# allow regression tests to be set by environment variable
if [ -z "$RSTART" ]; then
    export RSTART=1
fi
if [ -z "$REND" ]; then
    export REND=4
fi
# Launch the individual control and update runs, one-after-another
for jn in `seq ${RSTART} ${REND}`; do

   if [ $jn -le 2 ]; then
      export scripts=${scripts_updat:-$scripts}
      export fixgsi=${fixgsi_updat:-$fixgsi}
      export modulefiles=${modulefiles_updat:-$modulefiles}
      export ush=${ush_update:-$ush}
   else
      export scripts=${scripts_contrl:-$scripts}
      export fixgsi=${fixgsi_contrl:-$fixgsi}
      export modulefiles=${modulefiles_contrl:-$modulefiles}
      export ush=${ush_cntrl:-$ush}
   fi
   rm -f ${job[$jn]}.out

   /bin/sh $ush/$sub_cmd -m ${machine} -q $queue -j ${job[$jn]} -t ${topts[$jn]} -p ${popts[$jn]} -r ${ropts[$jn]} $scripts/${regtest}.sh

   if [ $debug == ".true." ]; then break; fi
   $scripts/regression_wait.sh ${job[$jn]} ${rcname} $check_resource
   rc=$?
   if [ $rc -ne 0 ]; then
     exit 1
   fi
   done
# When all done, test the results of the regression test
if [ "$debug" == ".false." ]; then

   export scripts=${scripts_updat:-$scripts}

   if [ $regtest = 'global_enkf' ] || [ $regtest = 'rrfs_enkf_conv' ]; then
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
