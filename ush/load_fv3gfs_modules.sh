#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
    echo "Loading modules quietly..."
    set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

# Find module command and purge:
source "${HOMEgfs}/modulefiles/module-setup.sh.inc"

# Source versions file for runtime
source "${HOMEgfs}/versions/run.ver"

# Load our modules:
module use "${HOMEgfs}/modulefiles"

if [[ -d /lfs/f1 ]]; then
  # We are on WCOSS2 (Cactus or Dogwood)
  module load module_base.wcoss2
elif [[ -d /mnt/lfs1 ]] ; then
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
elif [[ -d /data/prod ]] ; then
  # We are on SSEC S4
  module load module_base.s4
else
  echo WARNING: UNKNOWN PLATFORM
fi

module list

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

# If this function exists in the environment, run it; else do not
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
fi
