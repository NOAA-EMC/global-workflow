#!/bin/sh

###############################################################
# Setup runtime environment by loading modules
###############################################################

ulimit_s=$( ulimit -S -s )

# Find module command and purge:
source "$HOMEgfs/modulefiles/module-setup.sh.inc" 
source "$HOMEgfs/ush/get_platform.sh"

platform=$( get_platform )

moduledir=
modulelist=
case "${platform}" in
  hera)
    moduledir="$HOMEgfs/sorc/ufs_coupled.fd/modulefiles"
    target=${platform}.intel
    if [[ -r ${moduledir}/ufs_${target} ]] ; then
      modulelist=ufs_${target}
      if [[ "$( grep UFS_GOCART ${HOMEgfs}/sorc/ufs_coupled.fd/build/CMakeCache.txt 2>/dev/null | cut -d= -f2 )" = "ON" ]] ; then
        # add aerosols modulefile
        modulelist="${modulelist} ufs_aerosols_${target}"
      fi
    else
      moduledir="$HOMEgfs/modulefiles"
      modulelist="module_base.${platform}"
    fi
    ;;
  unknown)
    echo "Unknown platform"
    exit 1
    ;;
  *)
    moduledir="$HOMEgfs/modulefiles"
    modulelist="module_base.${platform}"
    ;;
esac

# Load our modules:
module use ${moduledir}
module load ${modulelist}

# Restore stack soft limit:
ulimit -S -s "$ulimit_s"
unset ulimit_s
