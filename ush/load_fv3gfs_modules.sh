#! /usr/bin/env bash

###############################################################
if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
    echo "Loading modules quietly..BS."
    set -x
fi

set -x
echo 'pjj900'
uname -n

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

## Find module command and purge:
#source "${HOMEgfs}/ush/detect_machine.sh"
#source "${HOMEgfs}/ush/module-setup.sh"

# Source versions file for runtime
#source "${HOMEgfs}/versions/run.ver"

# Load our modules:
#module use "${HOMEgfs}/modulefiles"

#case "${MACHINE_ID}" in
#  "wcoss2" | "hera" | "orion" | "hercules" | "gaea" | "jet" | "s4" | "noaacloud")
#    module load "module_base.${MACHINE_ID}"
#    ;;
#  *)
#    echo "WARNING: UNKNOWN PLATFORM"
#    ;;
#esac

#module list

# pjj - use this to load modules (spack-stack )
ROOT_DIR=${HOMEgfs}/sorc
set +x
#Determine machine and load modules
# pjj - use consistent module setup (spack-stack)
source "${ROOT_DIR}/ufs_model.fd/tests/detect_machine.sh"
source "${ROOT_DIR}/ufs_model.fd/tests/module-setup.sh"
module use "${ROOT_DIR}/ufs_model.fd/modulefiles"
module load "ufs_${MACHINE_ID}.intel"
module load prod_util
module list
set -x
echo $MACHINE_ID

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

# If this function exists in the environment, run it; else do not
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
fi
