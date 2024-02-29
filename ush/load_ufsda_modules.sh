#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
    echo "Loading modules quietly..."
    set +x
fi

# Read optional module argument, default is to use GDAS
MODS="GDAS"
if [[ $# -gt 0 ]]; then
  case "$1" in
    --eva)
      MODS="EVA"
      ;;
    --gdas)
      MODS="GDAS"
      ;;
    *)
      echo "Invalid option: $1" >&2
      exit 1
      ;;
  esac
fi

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

# Find module command and purge:
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# Load our modules:
module use "${HOMEgfs}/sorc/gdas.cd/modulefiles"

case "${MACHINE_ID}" in
  ("hera" | "orion" | "hercules")
    module load "${MODS}/${MACHINE_ID}"
    ncdump=$( command -v ncdump )
    NETCDF=$( echo "${ncdump}" | cut -d " " -f 3 )
    export NETCDF
    ;;
  ("wcoss2" | "acorn" | "jet" | "gaea" | "s4")
    echo WARNING: UFSDA NOT SUPPORTED ON THIS PLATFORM
    ;;  
  *)
    echo "WARNING: UNKNOWN PLATFORM"
    ;;
esac

module list
pip list

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

set_trace
