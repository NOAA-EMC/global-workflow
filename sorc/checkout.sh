#! /usr/bin/env bash

set +x
set -u

function usage() {
  cat << EOF
Clones and checks out external components necessary for
  global workflow. If the directory already exists, skip
  cloning and just check out the requested version (unless
  -c option is used).

Usage: ${BASH_SOURCE[0]} [-c][-h][-m ufs_hash][-o]
  -c:
    Create a fresh clone (delete existing directories)
  -h:
    Print this help message and exit
  -m ufs_hash:
    Check out this UFS hash instead of the default
  -o:
    Check out operational-only code (GTG and WAFS)
  -g:
    Check out GSI for GSI-based DA
  -u:
    Check out GDASApp for UFS-based DA
EOF
  exit 1
}

function checkout() {
  #
  # Clone or fetch repo, then checkout specific hash and update submodules
  #
  # Environment variables:
  #   topdir [default: $(pwd)]: parent directory to your checkout
  #   logdir [default: $(pwd)]: where you want logfiles written
  #   CLEAN [default: NO]:      whether to delete existing directories and create a fresh clone
  #
  # Usage: checkout <dir> <remote> <version>
  #
  #   Arguments
  #     dir:     Directory for the clone
  #     remote:  URL of the remote repository
  #     version: Commit to check out; should always be a speciifc commit (hash or tag), not a branch
  #
  #   Returns
  #     Exit code of last failed command, or 0 if successful
  #

  dir="$1"
  remote="$2"
  version="$3"
  recursive=${4:-"YES"}

  name=$(echo "${dir}" | cut -d '.' -f 1)
  echo "Performing checkout of ${name}"

  logfile="${logdir:-$(pwd)}/checkout_${name}.log"

  if [[ -f "${logfile}" ]]; then
    rm "${logfile}"
  fi

  cd "${topdir}" || exit 1
  if [[  -d "${dir}" && ${CLEAN} == "YES" ]]; then
    echo "|-- Removing existing clone in ${dir}"
    rm -Rf "${dir}"
  fi
  if [[ ! -d "${dir}" ]]; then
    echo "|-- Cloning from ${remote} into ${dir}"
    git clone "${remote}" "${dir}" >> "${logfile}" 2>&1
    status=$?
    if ((status > 0)); then
      echo "    WARNING: Error while cloning ${name}"
      echo
      return "${status}"
    fi
    cd "${dir}" || exit 1
  else
    # Fetch any updates from server
    cd "${dir}" || exit 1
    echo "|-- Fetching updates from ${remote}"
    git fetch
  fi
  echo "|-- Checking out ${version}"
  git checkout "${version}" >> "${logfile}" 2>&1
  status=$?
  if ((status > 0)); then
    echo "    WARNING: Error while checking out ${version} in ${name}"
    echo
    return "${status}"
  fi
  if [[ "${recursive}" == "YES" ]]; then
    echo "|-- Updating submodules (if any)"
    git submodule update --init --recursive >> "${logfile}" 2>&1
    status=$?
    if ((status > 0)); then
      echo "    WARNING: Error while updating submodules of ${name}"
      echo
      return "${status}"
    fi
  fi
  echo
  return 0
}

# Set defaults for variables toggled by options
export CLEAN="NO"
checkout_gsi="NO"
checkout_gdas="NO"
checkout_gtg="NO"
checkout_wafs="NO"

# Parse command line arguments
while getopts ":chgum:o" option; do
  case ${option} in
    c)
      echo "Received -c flag, will delete any existing directories and start clean"
      export CLEAN="YES"
      ;;
    g)
      echo "Received -g flag for optional checkout of GSI-based DA"
      checkout_gsi="YES"
      ;;
    h)  usage;;
    u)
      echo "Received -u flag for optional checkout of UFS-based DA"
      checkout_gdas="YES"
      ;;
    o)
      echo "Received -o flag for optional checkout of operational-only codes"
      checkout_gtg="YES"
      checkout_wafs="YES"
      ;;
    m)
      echo "Received -m flag with argument, will check out ufs-weather-model hash ${OPTARG} instead of default"
      ufs_model_hash=${OPTARG}
      ;;
    :)
      echo "option -${OPTARG} needs an argument"
      usage
      ;;
    *)
      echo "invalid option -${OPTARG}, exiting..."
      usage
      ;;
  esac
done
shift $((OPTIND-1))

topdir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
export topdir
export logdir="${topdir}/logs"
mkdir -p "${logdir}"

# The checkout version should always be a speciifc commit (hash or tag), not a branch
errs=0
checkout "gfs_utils.fd"    "https://github.com/NOAA-EMC/gfs-utils"              "0b8ff56"                    ; errs=$((errs + $?))
checkout "ufs_utils.fd"    "https://github.com/ufs-community/UFS_UTILS.git"     "4e673bf"                    ; errs=$((errs + $?))
checkout "ufs_model.fd"    "https://github.com/ufs-community/ufs-weather-model" "${ufs_model_hash:-2247060}" ; errs=$((errs + $?))
checkout "verif-global.fd" "https://github.com/NOAA-EMC/EMC_verif-global.git"   "c267780"                    ; errs=$((errs + $?))

if [[ ${checkout_gsi} == "YES" ]]; then
  checkout "gsi_enkf.fd" "https://github.com/NOAA-EMC/GSI.git" "113e307" "NO"; errs=$((errs + $?))
fi

if [[ ${checkout_gdas} == "YES" ]]; then
  checkout "gdas.cd" "https://github.com/NOAA-EMC/GDASApp.git" "aaf7caa"; errs=$((errs + $?))
fi

if [[ ${checkout_gsi} == "YES" || ${checkout_gdas} == "YES" ]]; then
  checkout "gsi_utils.fd"    "https://github.com/NOAA-EMC/GSI-Utils.git"   "322cc7b"; errs=$((errs + $?))
  checkout "gsi_monitor.fd"  "https://github.com/NOAA-EMC/GSI-Monitor.git" "45783e3"; errs=$((errs + $?))
  checkout "gldas.fd"        "https://github.com/NOAA-EMC/GLDAS.git"       "fd8ba62"; errs=$((errs + $?))
fi

if [[ ${checkout_wafs} == "YES" ]]; then
  checkout "gfs_wafs.fd" "https://github.com/NOAA-EMC/EMC_gfs_wafs.git" "014a0b8"; errs=$((errs + $?))
fi

if [[ ${checkout_gtg} == "YES" ]]; then
  ################################################################################
  # checkout_gtg
  ## yes: The gtg code at NCAR private repository is available for ops. GFS only.
  #       Only approved persons/groups have access permission.
  ## no:  No need to check out gtg code for general GFS users.
  ################################################################################

  echo "Checking out GTG extension for UPP"
  cd "${topdir}/ufs_model.fd/FV3/upp" || exit 1
  logfile="${logdir}/checkout_gtg.log"
  git -c submodule."post_gtg.fd".update=checkout submodule update --init --recursive >> "${logfile}" 2>&1
  status=$?
  if (( status > 0 )); then
    echo "WARNING: Error while checking out GTG"
    errs=$((errs + status))
  fi
fi

if (( errs > 0 )); then
  echo "WARNING: One or more errors encountered during checkout process, please check logs before building"
fi
echo
exit "${errs}"
