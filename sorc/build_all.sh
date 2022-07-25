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

Usage: $BASH_SOURCE [-a UFS_app][-c build_config][-h][-v]
  -a UFS_app:
    Build a specific UFS app instead of the default
  -c build_config:
    Selectively build based on the provided config instead of the default config
  -h:
    print this help message and exit
  -v:
    Execute all build scripts with -v option to turn on verbose where supported
EOF
  exit 1
}

_build_ufs_opt=""
_ops_opt=""
_verbose_opt=""
# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":a:c:hov" option; do
  case "${option}" in
    a) _build_ufs_opt+="-a ${OPTARG} ";;
    c) _partial_opt+="-c ${OPTARG} ";;
    h) _usage;;
    o) _ops_opt+="-o";;
    # s) _build_ufs_opt+="-s ${OPTARG} ";;
    v) _verbose_opt="-v";;
    \?)
      echo "[$BASH_SOURCE]: Unrecognized option: ${option}"
      usage
      ;;
    :)
      echo "[$BASH_SOURCE]: ${option} requires an argument"
      usage
      ;;
  esac
done

shift $((OPTIND-1))

build_dir=$(pwd)
logs_dir=$build_dir/logs
if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# GET MACHINE
#------------------------------------
target=""
source ./machine-setup.sh > /dev/null 2>&1

#------------------------------------
# INCLUDE PARTIAL BUILD
#------------------------------------
source ./partial_build.sh $_verbose_opt $_partial_opt

if [ $target = jet ]; then
  Build_gldas=false
  Build_gfs_util=false
  Build_ww3_prepost=false
fi

#------------------------------------
# Exception Handling Init
#------------------------------------
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
err=0

#------------------------------------
# build WW3 pre & post execs
#------------------------------------
$Build_ww3_prepost && {
  echo " .... Building WW3 pre and post execs .... "
  ./build_ww3prepost.sh ${_verbose_opt} ${_build_ufs_opt} > $logs_dir/build_ww3_prepost.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building WW3 pre/post processing."
    echo "The log file is in $logs_dir/build_ww3_prepost.log"
  fi
  ((err+=$rc))
}

#------------------------------------
# build forecast model
#------------------------------------
$Build_ufs_model && {
  echo " .... Building forecast model .... "
  ./build_ufs.sh $_verbose_opt ${_build_ufs_opt} > $logs_dir/build_ufs.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building UFS model."
    echo "The log file is in $logs_dir/build_ufs.log"
  fi
  ((err+=$rc))
}

#------------------------------------
# build GSI and EnKF - optional checkout
#------------------------------------
if [ -d gsi_enkf.fd ]; then
  $Build_gsi_enkf && {
  echo " .... Building gsi and enkf .... "
  ./build_gsi_enkf.sh $_ops_opt $_verbose_opt > $logs_dir/build_gsi_enkf.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building gsi_enkf."
    echo "The log file is in $logs_dir/build_gsi_enkf.log"
  fi
  ((err+=$rc))
}
else
  echo " .... Skip building gsi and enkf .... "
fi

#------------------------------------
# build gsi utilities
#------------------------------------
if [ -d gsi_utils.fd ]; then
  $Build_gsi_utils && {
  echo " .... Building gsi utilities .... "
  ./build_gsi_utils.sh $_ops_opt $_verbose_opt > $logs_dir/build_gsi_utils.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building gsi utilities."
    echo "The log file is in $logs_dir/build_gsi_utils.log"
  fi
  ((err+=$rc))
}
else
  echo " .... Skip building gsi utilities .... "
fi

#------------------------------------
# build gdas - optional checkout
#------------------------------------
if [ -d gdas.cd ]; then
  $Build_gdas  && {
  echo " .... Building GDASApp  .... "
  ./build_gdas.sh $_verbose_opt > $logs_dir/build_gdas.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building GDASApp."
    echo "The log file is in $logs_dir/build_gdas.log"
  fi
  ((err+=$rc))
}
else
  echo " .... Skip building GDASApp  .... "
fi

#------------------------------------
# build gsi monitor
#------------------------------------
if [ -d gsi_monitor.fd ]; then
  $Build_gsi_monitor && {
  echo " .... Building gsi monitor .... "
  ./build_gsi_monitor.sh $_ops_opt $_verbose_opt > $logs_dir/build_gsi_monitor.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building gsi monitor."
    echo "The log file is in $logs_dir/build_gsi_monitor.log"
  fi
  ((err+=$rc))
}
else
  echo " .... Skip building gsi monitor .... "
fi

#------------------------------------
# build UPP
#------------------------------------
$Build_upp && {
  echo " .... Building UPP .... "
  ./build_upp.sh $_ops_opt $_verbose_opt > $logs_dir/build_upp.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building UPP."
    echo "The log file is in $logs_dir/build_upp.log"
  fi
  ((err+=$rc))
}

#------------------------------------
# build ufs_utils
#------------------------------------
$Build_ufs_utils && {
  echo " .... Building ufs_utils .... "
  ./build_ufs_utils.sh $_verbose_opt > $logs_dir/build_ufs_utils.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building ufs_utils."
    echo "The log file is in $logs_dir/build_ufs_utils.log"
  fi
  ((err+=$rc))
}

#------------------------------------
# build gldas
#------------------------------------
if [ -d gldas.fd ]; then
  $Build_gldas && {
  echo " .... Building gldas .... "
  ./build_gldas.sh $_verbose_opt > $logs_dir/build_gldas.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building gldas."
    echo "The log file is in $logs_dir/build_gldas.log"
  fi
  ((err+=$rc))
}
else
  echo " .... Skip building gldas .... "
fi

#------------------------------------
# build gfs_wafs - optional checkout
#------------------------------------
if [ -d gfs_wafs.fd ]; then
  $Build_gfs_wafs  && {
  echo " .... Building gfs_wafs  .... "
  ./build_gfs_wafs.sh $_verbose_opt > $logs_dir/build_gfs_wafs.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building gfs_wafs."
    echo "The log file is in $logs_dir/build_gfs_wafs.log"
  fi
  ((err+=$rc))
}
fi

#------------------------------------
# build workflow_utils
#------------------------------------
$Build_workflow_utils && {
  echo " .... Building workflow_utils .... "
  target=$target ./build_workflow_utils.sh $_verbose_opt > $logs_dir/build_workflow_utils.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building workflow_utils."
    echo "The log file is in $logs_dir/build_workflow_utils.log"
  fi
  ((err+=$rc))
}

#------------------------------------
# build gfs_util
#------------------------------------
$Build_gfs_util && {
  echo " .... Building gfs_util .... "
  ./build_gfs_util.sh $_verbose_opt > $logs_dir/build_gfs_util.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
      echo "Fatal error in building gfs_util."
      echo "The log file is in $logs_dir/build_gfs_util.log"
  fi
  ((err+=$rc))
}

#------------------------------------
# Exception Handling
#------------------------------------
[[ $err -ne 0 ]] && echo "FATAL BUILD ERROR: Please check the log file for detail, ABORT!"
$ERRSCRIPT || exit $err

echo;echo " .... Build system finished .... "

exit 0
