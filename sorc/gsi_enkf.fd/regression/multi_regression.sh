#!/bin/sh --login

regtests_all="global_4denvar
              rrfs_3denvar_rdasens
              hafs_4denvar_glbens
              hafs_3denvar_hybens
              rtma
              global_enkf"

#              rrfs_enkf_conv   : comment out RRFS enkf case for now
#                                 need to update EnKF code
regtests_debug="global_4denvar
                rrfs_3denvar_rdasens
                hafs_4denvar_glbens
                hafs_3denvar_hybens
                rtma
                global_enkf"

# Choose which regression test to run; by default, run all
regtests=${1:-$regtests_all}

for regtest in $regtests; do
    rm -f ${regtest}.out
    echo "Launching regression test: $regtest"
    /bin/sh regression_driver.sh $regtest >& ${regtest}.out &
    sleep 1
done

exit
