#!/bin/sh
set -x

mode="${1:-exclusive}"

mode="${1:-exclusive}"

mode="${1:-exclusive}"

###############################################################
# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )
ulimit -S -s 10000

# Find module command and purge:
source "$HOMEgfs/modulefiles/module-setup.sh.inc" 

# Load our modules:
module use "$HOMEgfs/modulefiles" 

if [[ -d /lfs3 ]] ; then
    # We are on NOAA Jet
    if [[ "$mode" == forecast ]] ; then
	module load module_fcst.jet 
    else
	module load module_base.jet 
    fi
elif [[ -d /scratch3 ]] ; then
    # We are on NOAA Theia
	module load module_base.theia 
elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
    # We are on NOAA Luna or Surge
	module load module_base.wcoss_c 
elif [[ -d /dcom && -d /hwrf ]] ; then
    # We are on NOAA Tide or Gyre
	module load module_base.wcoss 
elif [[ -d /glade ]] ; then
    # We are on NCAR Yellowstone
	module load module_base.cheyenne 
elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA.
    if [[ "$mode" == service ]] ; then
        module load module-service.gaea
    else
        module load module-run.gaea
        module load module_base.gaea 
    fi
else
    echo WARNING: UNKNOWN PLATFORM 
fi

echo "CHECKING which mpirun is getting loaded on Jet"
which mpirun
mpirun --version
mpirun echo hello world

# Restore stack soft limit:
ulimit -S -s "$ulimit_s"
unset ulimit_s
