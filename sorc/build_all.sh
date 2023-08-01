#! /usr/bin/env bash

set +x
#------------------------------------
# Exception handling is now included.
#
# USER DEFINED STUFF:
#
#------------------------------------

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------
function _usage() {
  cat << EOF
Builds all of the global-workflow components by calling the individual build
  scripts in sequence.

Usage: ${BASH_SOURCE[0]} [-a UFS_app][-c build_config][-h][-v][-w]
  -a UFS_app:
    Build a specific UFS app instead of the default
  -c build_config:
    Selectively build based on the provided config instead of the default config
  -h:
    print this help message and exit
  -v:
    Execute all build scripts with -v option to turn on verbose where supported
  -w: 
    Use unstructured wave grid 
EOF
  exit 1
}

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd "${script_dir}" || exit 1

_build_ufs_opt=""
_ops_opt=""
_verbose_opt=""
_partial_opt=""
_wave_unst=""
# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":a:c:hovw" option; do
  case "${option}" in
    a) _build_ufs_opt+="-a ${OPTARG} ";;
    c) _partial_opt+="-c ${OPTARG} ";;
    h) _usage;;
    o) _ops_opt+="-o";;
    v) _verbose_opt="-v";;
    w) _wave_unst="-w";;
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      usage
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      usage
      ;;
  esac
done

shift $((OPTIND-1))

logs_dir="${script_dir}/logs"
if [[ ! -d "${logs_dir}" ]]; then
  echo "Creating logs folder"
  mkdir "${logs_dir}" || exit 1
fi

# Check final exec folder exists
if [[ ! -d "../exec" ]]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# GET MACHINE
#------------------------------------
export COMPILER="intel"
source gfs_utils.fd/ush/detect_machine.sh
source gfs_utils.fd/ush/module-setup.sh
if [[ -z "${MACHINE_ID}" ]]; then
  echo "FATAL: Unable to determine target machine"
  exit 1
fi

#------------------------------------
# SOURCE BUILD VERSION FILES
#------------------------------------
# TODO: Commented out until components aligned for build
#source ../versions/build.ver

#------------------------------------
# INCLUDE PARTIAL BUILD
#------------------------------------
# Turn off some shellcheck warnings because we want to have
#   variables with multiple arguments.
# shellcheck disable=SC2086,SC2248
source ./partial_build.sh ${_verbose_opt} ${_partial_opt}
# shellcheck disable=

#------------------------------------
# Exception Handling Init
#------------------------------------
# Disable shellcheck warning about single quotes not being substituted.
# shellcheck disable=SC2016
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
# shellcheck disable=
err=0

#------------------------------------
# build gfs_utils
#------------------------------------
if [[ ${Build_gfs_utils} == 'true' ]]; then
  echo " .... Building gfs_utils .... "
    # shellcheck disable=SC2086,SC2248
  ./build_gfs_utils.sh ${_verbose_opt} > "${logs_dir}/build_gfs_utils.log" 2>&1
    # shellcheck disable=
  rc=$?
  if (( rc != 0 )) ; then
    echo "Fatal error in building gfs_utils."
    echo "The log file is in ${logs_dir}/build_gfs_utils.log"
  fi
  err=$((err + rc))
fi

#------------------------------------
# build WW3 pre & post execs
#------------------------------------
if [[ ${Build_ww3_prepost} == "true" ]]; then
  echo " .... Building WW3 pre and post execs .... "
  # shellcheck disable=SC2086,SC2248
  ./build_ww3prepost.sh ${_verbose_opt} ${_build_ufs_opt} ${_wave_unst} > "${logs_dir}/build_ww3_prepost.log" 2>&1
  # shellcheck disable=
  rc=$?
  if (( rc != 0 )) ; then
    echo "Fatal error in building WW3 pre/post processing."
    echo "The log file is in ${logs_dir}/build_ww3_prepost.log"
  fi
  err=$((err + rc))
fi

#------------------------------------
# build forecast model
#------------------------------------
if [[ ${Build_ufs_model} == 'true' ]]; then
  echo " .... Building forecast model .... "
  # shellcheck disable=SC2086,SC2248
  ./build_ufs.sh ${_verbose_opt} ${_build_ufs_opt} ${_wave_unst} > "${logs_dir}/build_ufs.log" 2>&1
  # shellcheck disable=
  rc=$?
  if (( rc != 0 )) ; then
    echo "Fatal error in building UFS model."
    echo "The log file is in ${logs_dir}/build_ufs.log"
  fi
  err=$((err + rc))
fi

#------------------------------------
# build GSI and EnKF - optional checkout
#------------------------------------
if [[ -d gsi_enkf.fd ]]; then
  if [[ ${Build_gsi_enkf} == 'true' ]]; then
    echo " .... Building gsi and enkf .... "
    # shellcheck disable=SC2086,SC2248
    ./build_gsi_enkf.sh ${_ops_opt} ${_verbose_opt} > "${logs_dir}/build_gsi_enkf.log" 2>&1
    # shellcheck disable=
    rc=$?
    if (( rc != 0 )) ; then
      echo "Fatal error in building gsi_enkf."
      echo "The log file is in ${logs_dir}/build_gsi_enkf.log"
    fi
    err=$((err + rc))
  fi
else
  echo " .... Skip building gsi and enkf .... "
fi

#------------------------------------
# build gsi utilities
#------------------------------------
if [[ -d gsi_utils.fd ]]; then
  if [[ ${Build_gsi_utils} == 'true' ]]; then
    echo " .... Building gsi utilities .... "
    # shellcheck disable=SC2086,SC2248
    ./build_gsi_utils.sh ${_ops_opt} ${_verbose_opt} > "${logs_dir}/build_gsi_utils.log" 2>&1
    # shellcheck disable=
    rc=$?
    if (( rc != 0 )) ; then
      echo "Fatal error in building gsi utilities."
      echo "The log file is in ${logs_dir}/build_gsi_utils.log"
    fi
    err=$((err + rc))
  fi
else
  echo " .... Skip building gsi utilities .... "
fi

#------------------------------------
# build gdas - optional checkout
#------------------------------------
if [[ -d gdas.cd ]]; then
  if [[ ${Build_gdas} == 'true' ]]; then
    echo " .... Building GDASApp  .... "
    # shellcheck disable=SC2086,SC2248
    ./build_gdas.sh ${_verbose_opt} > "${logs_dir}/build_gdas.log" 2>&1
    # shellcheck disable=
    rc=$?
    if (( rc != 0 )) ; then
      echo "Fatal error in building GDASApp."
      echo "The log file is in ${logs_dir}/build_gdas.log"
    fi
    err=$((err + rc))
  fi
else
  echo " .... Skip building GDASApp  .... "
fi

#------------------------------------
# build gsi monitor
#------------------------------------
if [[ -d gsi_monitor.fd ]]; then
  if [[ ${Build_gsi_monitor} == 'true' ]]; then
    echo " .... Building gsi monitor .... "
    # shellcheck disable=SC2086,SC2248
    ./build_gsi_monitor.sh ${_ops_opt} ${_verbose_opt} > "${logs_dir}/build_gsi_monitor.log" 2>&1
    # shellcheck disable=
    rc=$?
    if (( rc != 0 )) ; then
      echo "Fatal error in building gsi monitor."
      echo "The log file is in ${logs_dir}/build_gsi_monitor.log"
    fi
    err=$((err + rc))
  fi
else
  echo " .... Skip building gsi monitor .... "
fi

#------------------------------------
# build UPP
#------------------------------------
if [[ ${Build_upp} == 'true' ]]; then
  echo " .... Building UPP .... "
  # shellcheck disable=SC2086,SC2248
  ./build_upp.sh ${_ops_opt} ${_verbose_opt} > "${logs_dir}/build_upp.log" 2>&1
  # shellcheck disable=
  rc=$?
  if (( rc != 0 )) ; then
    echo "Fatal error in building UPP."
    echo "The log file is in ${logs_dir}/build_upp.log"
  fi
  err=$((err + rc))
fi

#------------------------------------
# build ufs_utils
#------------------------------------
if [[ ${Build_ufs_utils} == 'true' ]]; then
  echo " .... Building ufs_utils .... "
  # shellcheck disable=SC2086,SC2248
  ./build_ufs_utils.sh ${_verbose_opt} > "${logs_dir}/build_ufs_utils.log" 2>&1
  # shellcheck disable=
  rc=$?
  if (( rc != 0 )) ; then
    echo "Fatal error in building ufs_utils."
    echo "The log file is in ${logs_dir}/build_ufs_utils.log"
  fi
  err=$((err + rc))
fi

#------------------------------------
# Exception Handling
#------------------------------------
if (( err != 0 )); then
  cat << EOF
BUILD ERROR: One or more components failed to build
  Check the associated build log(s) for details.
EOF
  ${ERRSCRIPT} || exit "${err}"
fi

echo;echo " .... Build system finished .... "

exit 0
