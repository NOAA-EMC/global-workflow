#!/bin/sh

set -x

. /scratch1/portfolios/NCEPDEV/da/save/$LOGNAME/EXP-port/scripts/regression_var.sh
#. regression_var.sh

if [[ "$arch" = "Linux" ]]; then

   # Submit jobs using sub_zeus wrapper.

   /bin/sh sub_zeus -j $global_T62_updat_exp1 -t 0:15:00 -p 6/6/0 $basedir/EXP-port/scripts/global_T62.sh

   /bin/sh sub_zeus -j $global_T62_updat_exp2 -t 0:12:00 -p 6/8/0 $basedir/EXP-port/scripts/global_T62.sh

elif [[ "$arch" = "AIX" ]]; then

   # Submit jobs using sub wrapper.

   /bin/sh sub -a GDAS-T2O -g dev -j $global_T62_updat_exp1 -p 32/1/N -r 110/1 -t 0:25:00 $basedir/EXP-port/scripts/global_T62.sh

   /bin/sh sub -a GDAS-T2O -g dev -j $global_T62_updat_exp2 -p 32/2/N -r 110/2 -t 0:15:00 $basedir/EXP-port/scripts/global_T62.sh

fi
