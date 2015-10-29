#!/bin/sh --login

set -x

machine=$REMOTEHOST

if [ -d /da ]; then
#For WCOSS
   echo "/da/save/$LOGNAME/trunk/scripts/regression_var.sh" > regression_var.out
elif [ -d /scratch4/NCEPDEV/da ]; then
#For Theia
   echo "/scratch4/NCEPDEV/da/save/$LOGNAME/trunk/scripts/regression_var.sh" > regression_var.out
fi

/bin/sh global_T62_regression.sh > global_T62.out &

/bin/sh global_T62_ozonly_regression.sh > global_T62_ozonly.out &

/bin/sh global_4dvar_T62_regression.sh > global_4dvar_T62.out &

/bin/sh global_hybrid_T126_regression.sh > global_hybrid_T126.out &

/bin/sh global_lanczos_T62_regression.sh > global_lanczos_T62.out &

/bin/sh global_nemsio_T62_regression.sh > global_nemsio_T62.out &

/bin/sh arw_netcdf_regression.sh > arw_netcdf.out &

/bin/sh arw_binary_regression.sh > arw_binary.out &

/bin/sh nmm_binary_regression.sh > nmm_binary.out &

/bin/sh nmm_netcdf_regression.sh > nmm_netcdf.out &

/bin/sh nmmb_nems_4denvar_regression.sh > nmmb_nems_4denvar.out &

/bin/sh rtma_regression.sh > rtma.out &

/bin/sh hwrf_nmm_d2_regression.sh > hwrf_nmm_d2.out &

/bin/sh hwrf_nmm_d3_regression.sh > hwrf_nmm_d3.out &

/bin/sh global_enkf_T62_regression.sh > global_enkf_T62.out &

exit
