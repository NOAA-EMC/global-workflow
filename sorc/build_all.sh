#!/bin/sh
set +x
#------------------------------------
# Exception handling is now included.
#
# USER DEFINED STUFF:
#
# USE_PREINST_LIBS: set to "true" to use preinstalled libraries.
#                   Anything other than "true"  will use libraries locally.
#------------------------------------

export USE_PREINST_LIBS="true"

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------

build_dir=`pwd`
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

. ./partial_build.sh

#------------------------------------
# Exception Handling Init
#------------------------------------
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
err=0

#------------------------------------
# build fv3
#------------------------------------
$Build_fv3gfs && {
echo " .... Building fv3 .... "
./build_fv3.sh > $logs_dir/build_fv3.log 2>&1
rc=$?
if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building fv3."
    echo "The log file is in $logs_dir/build_fv3.log"
fi
((err+=$rc))
}

#------------------------------------
# build WW3 pre & post execs 
#------------------------------------
$Build_ww3_prepost && {
echo " .... Building WW3 pre and post execs .... "
./build_ww3prepost.sh > $logs_dir/build_ww3_prepost.log 2>&1
rc=$?
if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building WW3 pre/post processing."
    echo "The log file is in $logs_dir/build_ww3_prepost.log"
fi
((err+=$rc))
}

#------------------------------------
# build gsi
#------------------------------------
$Build_gsi && {
echo " .... Building gsi .... "
./build_gsi.sh > $logs_dir/build_gsi.log 2>&1
rc=$?
if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building gsi."
    echo "The log file is in $logs_dir/build_gsi.log"
fi
((err+=$rc))
}

#------------------------------------
# build ncep_post
#------------------------------------
$Build_ncep_post && {
echo " .... Building ncep_post .... "
./build_ncep_post.sh > $logs_dir/build_ncep_post.log 2>&1
rc=$?
if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building ncep_post."
    echo "The log file is in $logs_dir/build_ncep_post.log"
fi
((err+=$rc))
}

#------------------------------------
# build ufs_utils
#------------------------------------
$Build_ufs_utils && {
echo " .... Building ufs_utils .... "
./build_ufs_utils.sh > $logs_dir/build_ufs_utils.log 2>&1
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
$Build_gldas && {
echo " .... Building gldas .... "
./build_gldas.sh > $logs_dir/build_gldas.log 2>&1
rc=$?
if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in building gldas."
    echo "The log file is in $logs_dir/build_gldas.log"
fi
((err+=$rc))
}

#------------------------------------
# build gfs_wafs - optional checkout 
#------------------------------------
if [ -d gfs_wafs.fd ]; then
  $Build_gfs_wafs  && {
  echo " .... Building gfs_wafs  .... "
  ./build_gfs_wafs.sh > $logs_dir/build_gfs_wafs.log 2>&1
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
target=$target ./build_workflow_utils.sh > $logs_dir/build_workflow_utils.log 2>&1
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
./build_gfs_util.sh > $logs_dir/build_gfs_util.log 2>&1
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
