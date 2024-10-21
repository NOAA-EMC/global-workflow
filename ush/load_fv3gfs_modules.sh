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
source "${HOMEgfs}/versions/run.${MACHINE_ID}.ver"

# Load our modules:
module use "${HOMEgfs}/modulefiles"

case "${MACHINE_ID}" in
  "noaacloud")
    #TODO this is a total kludge to get epic mount point for compute nodes
    #    to be the same as the login node.  This should be workng from in the
    #    ALLNODES section of the User Bootstrap of Parllel Works but it doen't
    #    on the Rokcky Clusters (works fine in the Centos 7 cluster)
    if [[ ! -d  /contrib-epic/EPIC ]]; then
      /contrib/Terry.McGuinness/SETUP/mount-epic-contrib.sh
      sudo systemctl daemon-reload
    fi
    module load "module_base.${MACHINE_ID}"
    ;;
  "wcoss2" | "hera" | "orion" | "hercules" | "gaea" | "jet" | "s4")
    module load "module_base.${MACHINE_ID}"
    ;;
  *)
    echo "WARNING: UNKNOWN PLATFORM"
    ;;
esac

module list

# Add wxflow to PYTHONPATH
wxflowPATH="${HOMEgfs}/ush/python"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush:${wxflowPATH}"
export PYTHONPATH

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

# If this function exists in the environment, run it; else do not
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
fi
