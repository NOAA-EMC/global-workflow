#!/bin/sh

###############################################################
# Setup runtime environment by loading modules
#
# This script accepts an optional argument for the task type.
# Acceptable types are: exclusive, forecast, service, shared
#
###############################################################

ulimit_s=$( ulimit -S -s )

# Find module command and purge:
source "$HOMEgfs/modulefiles/module-setup.sh.inc" 
source "$HOMEgfs/ush/get_platform.sh"

tasktype=
if [ $# -eq 1 ] ; then
  case "${1}" in
    exclusive|forecast|service|shared)
      tasktype="${1}"
      ;;
    *)
      echo "Unsupported task type"
      exit 1
      ;;
  esac
fi

platform=$( get_platform )

moduledir="$HOMEgfs/modulefiles"
modulelist="module_base.${platform}"

# Load our modules:
module use ${moduledir}
module load ${modulelist}

# Restore stack soft limit:
ulimit -S -s "$ulimit_s"
unset ulimit_s
