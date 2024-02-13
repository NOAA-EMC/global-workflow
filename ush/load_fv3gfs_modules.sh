#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
    echo "Loading modules quietly..."
    set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

# Find module command and purge:
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# Source versions file for runtime
source "${HOMEgfs}/versions/run.ver"

# Load our modules:
module use "${HOMEgfs}/modulefiles"

if [[ "${MACHINE_ID}" == "wcoss2" || "${MACHINE_ID}" == "acorn" ]]; then
  # We are on WCOSS2 (Cactus or Dogwood)
  module load module_base.wcoss2
elif [[ "${MACHINE_ID}" == "jet" ]] ; then
  # We are on NOAA Jet
  module load module_base.jet
elif [[ "${MACHINE_ID}" == "hera" ]] ; then
  # We are on NOAA Hera
  module load module_base.hera
elif [[ "${MACHINE_ID}" == "orion" ]] ; then
  # We are on MSU Orion
  module load module_base.orion
elif [[ "${MACHINE_ID}" == "hercules" ]] ; then  
  # We are on MSU Hercules
  module load module_base.hercules
elif [[ "${MACHINE_ID}" == "gaea" ]] ; then
  # We are on GAEA.
  module load module_base.gaea
elif [[ "${MACHINE_ID}" == "s4" ]] ; then
  # We are on SSEC S4
  module load module_base.s4
else
  echo WARNING: UNKNOWN PLATFORM # TODO: Should an exception be raised here?
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
