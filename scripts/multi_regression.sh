#!/bin/sh --login

echo "`pwd`/regression_var.sh" > regression_var.out

#regtests="global_hybrid_T126"
regtests="global_4denvar_T126"

for regtest in $regtests; do
    echo "Launching regression test: $regtest"
    /bin/sh regression_driver.sh $regtest >& $regtest.out &
    sleep 1
done

exit
