#!/bin/sh --login

regtests_all="global_T62
              global_T62_ozonly
              global_4dvar_T62
              global_hybrid_T126
              global_lanczos_T62
              global_nemsio_T62
              arw_netcdf
              arw_binary
              nmm_binary
              nmm_netcdf
              nmmb_nems_4denvar
              hwrf_nmm_d2
              hwrf_nmm_d3
              rtma
              global_enkf_T62"
                global_4dvar_T62
                global_hybrid_T126
                global_lanczos_T62
                global_nemsio_T62
                arw_netcdf
                arw_binary
                nmm_binary
                nmm_netcdf
                nmmb_nems_4denvar
                hwrf_nmm_d2
                hwrf_nmm_d3"

regtests_debug="global_T62
                global_4dvar_T62
                global_hybrid_T126
                global_lanczos_T62
                global_nemsio_T62
                arw_netcdf
                arw_binary
                nmm_binary
                nmm_netcdf
                nmmb_nems_4denvar
                hwrf_nmm_d2
                hwrf_nmm_d3"

# Choose which regression test to run; by default, run all
regtests=${1:-$regtests_all}

echo "`pwd`/regression_var.sh" > regression_var.out

for regtest in $regtests; do
    rm -f ${regtest}.out
    echo "Launching regression test: $regtest"
    /bin/sh regression_driver.sh $regtest >& ${regtest}.out &
    sleep 1
done

exit
