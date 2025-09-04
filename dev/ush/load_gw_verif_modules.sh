#! /usr/bin/env bash

###############################################################
if [[ "$-" == *x* ]]; then
  set_x=YES
else
  set_x=NO
fi

if [[ "${DEBUG_WORKFLOW:-NO}" == "NO" ]]; then
  echo "Loading modules quietly..."
  set +x
fi

# Setup runtime environment by loading modules
ulimit_s=$( ulimit -S -s )

# Test if HOMEgfs is defined.  If not, then try to determine it with git rev-parse
_unset_homegfs="NO"
if [[ -z ${HOMEgfs+x} ]]; then
  echo "INFO HOMEgfs is not defined.  Attempting to find the global-workflow root directory"
  # HOMEgfs will be removed from the environment at the end of this script
  _unset_homegfs="YES"

  script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
  HOMEgfs=$(cd "${script_dir}" && git rev-parse --show-toplevel)
  export HOMEgfs
  err=$?
  if [[ ${err} -ne 0 ]]; then
    is_git_dir=$( cd -- "${script_dir}" &> /dev/null && git rev-parse --is-inside-work-tree)
    git_stat=$?
    if [[ ${git_stat} -ne 0 || ${is_git_dir} != "true" ]]; then
      echo "FATAL ERROR unable to determine the root because it is not a git repository."
    else
      echo "FATAL ERROR unable to determine the root because git rev-parse --show-toplevel failed for an unknown reason"
    fi
    echo "            Unable to load modules.  Exiting"
    exit 1
  fi
fi

# Find module command and purge:
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# Source versions file for runtime
if [[ -f "${HOMEgfs}/versions/run.ver" ]]; then
    source "${HOMEgfs}/versions/run.ver"
else
    echo "FATAL ERROR ${HOMEgfs}/versions/run.ver does not exist!"
    echo "HINT: Run link_workflow.sh first."
    exit 1
fi

# Load our modules:
module use "${HOMEgfs}/modulefiles"

case "${MACHINE_ID}" in
"wcoss2")
  target_module="gw_run.${MACHINE_ID}"
  ;;
"hera")
  target_module="gw_verif.${MACHINE_ID}"
  ;;
*)
  echo "WARNING: UNKNOWN/UNSUPPORTED PLATFORM: '${MACHINE_ID}'"
  target_module="gw_run.${MACHINE_ID}"
  ;;
esac

module load "${target_module}"
export err=$?
if [[ ${err} -ne 0 ]]; then
  echo "FATAL ERROR: Failed to load ${target_module}"
  exit 1
fi

module list

# If this function exists in the environment, run it; else set -x if it was set on entering this script
ftype=$(type -t set_trace || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_trace
elif [[ "${set_x}" == "YES" ]]; then
  set -x
fi

# Set up the PYTHONPATH to include wxflow from HOMEgfs
if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
  PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/sorc/wxflow/src"
fi

# Add HOMEgfs/ush/python to PYTHONPATH
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush/python"
export PYTHONPATH

# Restore stack soft limit:
ulimit -S -s "${ulimit_s}"
unset ulimit_s

# Unset HOMEgfs if it was not set at the beginning of this script
if [[ ${_unset_homegfs} == "YES" ]]; then
    unset HOMEgfs
fi
