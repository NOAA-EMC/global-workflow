#!/bin/sh

###############################################################
# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

set +x

# Find module command and purge:
source "$HOMEgfs/modulefiles/module-setup.sh.inc"

# Load our modules:
module use "$HOMEgfs/modulefiles"

if [[ -d /lfs3 ]] ; then
  # We are on NOAA Jet
  module load module_base.jet
elif [[ -d /scratch1 ]] ; then
  # We are on NOAA Hera
  module load module_base.hera
elif [[ -d /work ]] ; then
  # We are on MSU Orion
  module load module_base.orion
elif [[ -d /glade ]] ; then
  # We are on NCAR Yellowstone
  module load module_base.cheyenne
elif [[ -d /lustre && -d /ncrc ]] ; then
  # We are on GAEA.
  module load module_base.gaea
else
  echo WARNING: UNKNOWN PLATFORM
fi

set -x

# Restore stack soft limit:
ulimit -S -s "$ulimit_s"
unset ulimit_s
