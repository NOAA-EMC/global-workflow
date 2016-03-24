#!/bin/sh --login

set -x

if [ -d /da ]; then # WCOSS
   machine=WCOSS
   echo "/da/save/$LOGNAME/gsi/branches/EXP-update/scripts/regression_var.sh" > regression_var.out
elif [ -d /scratch4/NCEPDEV/da ]; then # Theia
   machine=Theia
   echo "/scratch4/NCEPDEV/da/save/$LOGNAME/gsi/branches/EXP-update/scripts/regression_var.sh" > regression_var.out
fi

regexpts="global_T62 global_T62_ozonly global_4dvar_T62 global_hybrid_T126 global_lanczos_T62 global_nemsio_T62 arw_netcdf arw_binary nmm_binary nmm_netcdf hwrf_nmm_d2 hwrf_nmm_d3 rtma global_enkf_T62"

for regtest in $regexpts; do
    /bin/sh regression_driver.sh $regtest > $regtest.out &
done

exit
