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
  hera.intel)
    moduledir="$HOMEgfs/sorc/ufs_coupled.fd/modulefiles"
    if [[ -r ${moduledir}/${platform}/fv3 ]] ; then
      moduledir="$HOMEgfs/sorc/ufs_coupled.fd/modulefiles/${platform}"
      modulelist=fv3
    elif [[ -r ${moduledir}/ufs_${platform} ]] ; then
      modulelist=ufs_${platform}
      if [[ "$( grep UFS_GOCART ${HOMEgfs}/sorc/ufs_coupled.fd/build/CMakeCache.txt 2>/dev/null | cut -d= -f2 )" = "ON" ]] ; then
        # add aerosols modulefile
        modulelist="${modulelist} ufs_aerosols_${platform}"
      fi
    else
      echo "Unable to load modulefiles on ${platform}"
      exit 1
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
