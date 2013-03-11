#!/bin/sh --login

set -x

if [[ "`uname -s | awk '{print $1}'`" = 'Linux' ]]; then
   echo "/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/EXP-port/scripts/regression_var.sh" > regression_var.out
elif [[ "`uname -s | awk '{print $1}'`" = 'AIX' ]]; then
   echo "/global/save/wx20ml/EXP-port/scripts/regression_var.sh" > regression_var.out
fi

/bin/sh global_T62_regression.sh > global_T62.out &

/bin/sh global_hybrid_T126_regression.sh > global_hybrid_T126.out &

/bin/sh arw_netcdf_regression.sh > arw_netcdf.out &

/bin/sh arw_binary_regression.sh > arw_binary.out &

/bin/sh nmm_binary_regression.sh > nmm_binary.out &

/bin/sh nmm_netcdf_regression.sh > nmm_netcdf.out &

/bin/sh nmmb_nems_regression.sh > nmmb_nems.out &

/bin/sh rtma_regression.sh > rtma.out &

exit
