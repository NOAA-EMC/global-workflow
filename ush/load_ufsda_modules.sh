#! /usr/bin/env bash

###############################################################
export DEBUG_WORKFLOW="${DEBUG_WORKFLOW:-NO}"
if [[ "${DEBUG_WORKFLOW}" == "NO" ]]; then
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
source "${HOMEgfs}/modulefiles/module-setup.sh.inc"

# Load our modules:
module use "${HOMEgfs}/sorc/gdas.cd/modulefiles"

if [[ -d /lfs/f1 ]]; then
  # We are on WCOSS2 (Cactus or Dogwood)
  echo WARNING: UFSDA NOT SUPPORTED ON THIS PLATFORM
elif [[ -d /lfs3 ]] ; then
  # We are on NOAA Jet
  echo WARNING: UFSDA NOT SUPPORTED ON THIS PLATFORM
elif [[ -d /scratch1 ]] ; then
  # We are on NOAA Hera
  module load "${MODS}/hera"
  if [[ "${DEBUG_WORKFLOW}" == "YES" ]] ; then
     module list
     pip list
  fi
elif [[ -d /work ]] ; then
  # We are on MSU Orion
  module load "${MODS}/orion"
  if [[ "${DEBUG_WORKFLOW}" == "YES" ]] ; then
     module list
     pip list
  fi
elif [[ -d /glade ]] ; then
  # We are on NCAR Yellowstone
  echo WARNING: UFSDA NOT SUPPORTED ON THIS PLATFORM
elif [[ -d /lustre && -d /ncrc ]] ; then
  # We are on GAEA.
  echo WARNING: UFSDA NOT SUPPORTED ON THIS PLATFORM
elif [[ -d /data/prod ]] ; then
  # We are on SSEC S4
  echo WARNING: UFSDA NOT SUPPORTED ON THIS PLATFORM
else
  echo WARNING: UNKNOWN PLATFORM
fi

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

set_trace
