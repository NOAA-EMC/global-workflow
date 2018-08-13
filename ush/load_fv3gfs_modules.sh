#!/bin/sh
#set -x

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
	module load module_base.jet 
elif [[ -d /scratch3 ]] ; then
    # We are on NOAA Theia
	module load module_base.theia 
elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
    # We are on NOAA Luna or Surge
	module load module_base.wcoss_c 
elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then
    # We are on NOAA Mars or Venus
	module load module_base.wcoss_dell_p3 
elif [[ -d /dcom && -d /hwrf ]] ; then
    # We are on NOAA Tide or Gyre
	module load module_base.wcoss 
elif [[ -d /glade ]] ; then
    # We are on NCAR Yellowstone
	module load module_base.cheyenne 
elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA.
	module load module_base.gaea 
else
    echo WARNING: UNKNOWN PLATFORM 
fi

# Restore stack soft limit:
ulimit -S -s "$ulimit_s"
unset ulimit_s
