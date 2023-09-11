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
  # set NETCDF variable based on ncdump location
  NETCDF=$( which ncdump )
  export NETCDF
  # prod_util stuff, find a better solution later...
  module use /scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/compiler/intel/2022.1.2/
  module load prod_util
elif [[ -d /work ]] ; then
  # We are on MSU Orion
  # prod_util stuff, find a better solution later...
  #module use /apps/contrib/NCEP/hpc-stack/libs/hpc-stack/modulefiles/compiler/intel/2022.1.2/
  #module load prod_util
  export UTILROOT=/work2/noaa/da/python/opt/intel-2022.1.2/prod_util/1.2.2
  export MDATE=/work2/noaa/da/python/opt/intel-2022.1.2/prod_util/1.2.2/bin/mdate
  export NDATE=/work2/noaa/da/python/opt/intel-2022.1.2/prod_util/1.2.2/bin/ndate
  export NHOUR=/work2/noaa/da/python/opt/intel-2022.1.2/prod_util/1.2.2/bin/nhour
  export FSYNC=/work2/noaa/da/python/opt/intel-2022.1.2/prod_util/1.2.2/bin/fsync_file
  module load "${MODS}/orion"
  # set NETCDF variable based on ncdump location
  ncdump=$( which ncdump )
  NETCDF=$( echo "${ncdump}" | cut -d " " -f 3 )
  export NETCDF
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

module list
pip list

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

set_trace
