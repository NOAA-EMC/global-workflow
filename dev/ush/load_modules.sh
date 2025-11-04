#! /usr/bin/env bash

###############################################################
# Consolidated module loading script for global-workflow
# Usage: source load_modules.sh [module_type]
# where module_type can be: run, gsi, verif, ufsda, ufswm, setup
# Default module_type is 'run'
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

# Parse module type argument
MODULE_TYPE="${1:-run}"

# For backwards compatibility, handle ufsda options
UFSDA_MODS="GDAS"
if [[ "${MODULE_TYPE}" == "--eva" ]]; then
  MODULE_TYPE="ufsda"
  UFSDA_MODS="EVA"
elif [[ "${MODULE_TYPE}" == "--gdas" ]]; then
  MODULE_TYPE="ufsda"
  UFSDA_MODS="GDAS"
fi

# Setup runtime environment by loading modules
ulimit_s=$(ulimit -S -s)

# Test if HOMEgfs is defined.  If not, then try to determine it with git rev-parse
_unset_homegfs="NO"
if [[ -z ${HOMEgfs+x} ]]; then
  echo "INFO: HOMEgfs is not defined.  Attempting to find the global-workflow root directory"
  # HOMEgfs will be removed from the environment at the end of this script
  _unset_homegfs="YES"

  script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
  HOMEgfs=$(cd "${script_dir}" && git rev-parse --show-toplevel)
  export HOMEgfs
  err=$?
  if [[ ${err} -ne 0 ]]; then
    is_git_dir=$(cd -- "${script_dir}" &> /dev/null && git rev-parse --is-inside-work-tree)
    git_stat=$?
    if [[ ${git_stat} -ne 0 || ${is_git_dir} != "true" ]]; then
      echo "FATAL ERROR: unable to determine the root because it is not a git repository."
    else
      echo "FATAL ERROR: unable to determine the root because git rev-parse --show-toplevel failed for an unknown reason"
    fi
    echo "FATAL ERROR: Unable to load modules.  Exiting"
    exit 1
  fi
fi

# Find module command and purge:
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

# Handle different module types
case "${MODULE_TYPE}" in
  "ufswm")
    # UFS Weather Model modules - special handling
    module use "${HOMEgfs}/sorc/ufs_model.fd/modulefiles"
    module load "ufs_${MACHINE_ID}.intel"
    export err=$?
    if [[ ${err} -ne 0 ]]; then
      echo "FATAL ERROR: Failed to load ufs_${MACHINE_ID}.intel"
      exit 1
    fi
    module load prod_util
    if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
      module load cray-pals
      module load cfp
      module load libjpeg
      module load craype-network-ucx
      module load cray-mpich-ucx
      module load python/3.8.6
      module load wgrib2
    else
      export UTILROOT=${prod_util_ROOT}
      source "${HOMEgfs}/versions/run.ver"
      module load "wgrib2/${wgrib2_ver}"
    fi
    export WGRIB2=wgrib2

    module list
    unset MACHINE_ID
    ;;

  "ufsda")
    # UFSDA modules - special handling
    module use "${HOMEgfs}/sorc/gdas.cd/modulefiles"

    case "${MACHINE_ID}" in
      ("hera" | "orion" | "hercules" | "wcoss2" | "gaeac5" | "gaeac6" | "ursa" | "noaacloud")
        #TODO: Remove LMOD_TMOD_FIND_FIRST line when spack-stack on WCOSS2
        if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
          export LMOD_TMOD_FIND_FIRST=yes
          # TODO: Add path to GDASApp libraries and cray-mpich as temporary patches
          export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${HOMEgfs}/sorc/gdas.cd/build/lib"
          # TODO: Remove LD_LIBRARY_PATH line as soon as permanent solution is available
          export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/cray/pe/mpich/8.1.29/ofi/intel/2022.1/lib"
        fi
        module load "${UFSDA_MODS}/${MACHINE_ID}"
        export err=$?
        if [[ ${err} -ne 0 ]]; then
          echo "FATAL ERROR: Failed to load ${UFSDA_MODS}/${MACHINE_ID}"
          exit 1
        fi
        ncdump=$(command -v ncdump)
        NETCDF=$(echo "${ncdump}" | cut -d " " -f 3)
        export NETCDF
        ;;
      ("acorn")
        echo WARNING: UFSDA NOT SUPPORTED ON 'acorn'
        ;;
      *)
        echo "WARNING: UNKNOWN PLATFORM"
        ;;
    esac

    module list

    ftype=$(type -t set_trace || echo "")
    if [[ "${ftype}" == "function" ]]; then
      set_trace
    elif [[ "${set_x}" == "YES" ]]; then
      set -x
    fi

    pip list

    # Detect the Python major.minor version
    _regex="[0-9]+\.[0-9]+"
    # shellcheck disable=SC2312
    if [[ $(python --version) =~ ${_regex} ]]; then
      export PYTHON_VERSION="${BASH_REMATCH[0]}"
    else
      echo "FATAL ERROR: Could not detect the python version"
      exit 1
    fi

    ###############################################################
    # setup python path for ioda utilities
    # TODO: a better solution should be created for setting paths to package python scripts
    # shellcheck disable=SC2311
    pyiodaPATH="${HOMEgfs}/sorc/gdas.cd/build/lib/python${PYTHON_VERSION}/"
    pybufrPATH="${HOMEgfs}/sorc/gdas.cd/build/lib/python${PYTHON_VERSION}/site-packages/"
    PYTHONPATH="${pyiodaPATH}:${pybufrPATH}${PYTHONPATH:+:${PYTHONPATH}}"
    export PYTHONPATH
    ;;

  "run" | "gsi" | "verif" | "setup" | "upp")

    # Test that the version file exists
    if [[ ! -f "${HOMEgfs}/versions/run.ver" ]]; then
      echo "FATAL ERROR: ${HOMEgfs}/versions/run.ver does not exist!"
      echo "HINT: Run link_workflow.sh first."
      exit 1
    fi

    # Load our modules:
    module use "${HOMEgfs}/modulefiles"

    # Determine target module based on type and machine
    target_module="gw_${MODULE_TYPE}.${MACHINE_ID}"
    
    # Check if the target module file exists, fall back to gw_run if not
    if ! module is-avail "${target_module}" 2>/dev/null; then
      if [[ "${MODULE_TYPE}" != "run" ]]; then
        echo "INFO: ${target_module} module not available, falling back to gw_run.${MACHINE_ID}"
        mod_type="run"
      fi
      target_module="gw_run.${MACHINE_ID}"
    else
      mod_type="${MODULE_TYPE}"
    fi

    # Source versions file (except for upp)
    if [[ "${mod_type}" != "upp" ]]; then
      source "${HOMEgfs}/versions/run.ver"
    fi

    if [[ -n "${target_module}" ]]; then
      module load "${target_module}"
      export err=$?
      if [[ ${err} -ne 0 ]]; then
        echo "FATAL ERROR: Failed to load ${target_module}"
        exit 1
      fi
    else
      echo "FATAL ERROR: Could not determine target module for MODULE_TYPE='${MODULE_TYPE}' and MACHINE_ID='${MACHINE_ID}'"
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
    ;;

  *)
    echo "FATAL ERROR: Unknown module type '${MODULE_TYPE}'"
    echo "Valid types: run, gsi, verif, ufsda, ufswm, setup"
    exit 1
    ;;
esac

# Set up the PYTHONPATH to include wxflow from HOMEgfs
if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
  PYTHONPATH="${HOMEgfs}/sorc/wxflow/src${PYTHONPATH:+:${PYTHONPATH}}"
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
