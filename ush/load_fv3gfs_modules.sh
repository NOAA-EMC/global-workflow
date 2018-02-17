#!/bin/sh

set -x

###############################################################
# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )
ulimit -S -s 10000

# Find module command and purge:
source "$HOMEgfs/modulefiles/module-setup.sh.inc" 2> /dev/null

# Load our modules:
module use "$HOMEgfs/modulefiles" 2> /dev/null

if [[ -d /lfs3 ]] ; then
    # We are on NOAA Jet
	module load module_base.jet 2> /dev/null
elif [[ -d /scratch3 ]] ; then
    # We are on NOAA Theia
	module load module_base.theia 2> /dev/null
elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
    # We are on NOAA Luna or Surge
	module load module_base.wcoss_c 2> /dev/null
elif [[ -d /dcom && -d /hwrf ]] ; then
    # We are on NOAA Tide or Gyre
	module load module_base.wcoss 2> /dev/null
elif [[ -d /glade ]] ; then
    # We are on NCAR Yellowstone
	module load module_base.cheyenne 2> /dev/null
elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA.
	module load module_base.gaea 2> /dev/null
else
    echo WARNING: UNKNOWN PLATFORM 1>&2
fi

# Restore stack soft limit:
ulimit -S -s "$ulimit_s"
unset ulimit_s
