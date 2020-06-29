#!/bin/sh
set +x
#------------------------------------
# Run global-workflow installation
#
# 1 - run checkout
# 2 - run build
# 3 - run linking
#------------------------------------

while getopts ":m:t:CBL" option;
do
 case $option in
  m)
   echo received -m with $OPTARG
   mode=$OPTARG
   ;;
  t)
   echo received -t with $OPTARG
   TOPDIR=$OPTARG
   ;;
  C)
   echo received -C
   CHECKOUT="YES"
   ;;
  B)
   echo received -B
   BUILD="YES"
   ;;
  L)
   echo received -L
   LINK="YES"
   ;;
  :)
   echo "option -$OPTARG needs an argument"
   ;;
  *)
   echo "invalid option -$OPTARG, exiting..."
   exit
   ;;
 esac
done

rc=0 # Set return code
ME_VER=manic-v1.1.8 # Set manage_externals version

mode=${mode:-'dev'} # "dev" - not production installs; "prod" - NCO production installation
TOPDIR=${TOPDIR:-`pwd`}
CHECKOUT=${CHECKOUT:-"NO"}
BUILD=${BUILD:-"NO"}
LINK=${LINK:-"NO"}
# If no feature flags set, set all to YES
if [ $CHECKOUT = "NO" -a $BUILD = "NO" -a $LINK = "NO" ]; then
  CHECKOUT="YES"; BUILD="YES"; LINK="YES"
fi

#------------------------------------
# GET MACHINE
#------------------------------------
target=""
source $TOPDIR/sorc/machine-setup.sh > /dev/null 2>&1

#------------------------------------
# RUN CHECKOUT
#------------------------------------
if [ $CHECKOUT = "YES" ]; then

  # Set manage_externals path
  if [ $target = "wcoss_dell_p3" ]; then
    ME_INSTALL=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/manage_externals/$ME_VER
  elif [ $target = "wcoss_cray" ]; then
    ME_INSTALL=/gpfs/hps3/emc/global/noscrub/emc.glopara/git/manage_externals/$ME_VER
  elif [ $target = "hera" ]; then
    ME_INSTALL=/scratch1/NCEPDEV/global/glopara/git/manage_externals/$ME_VER
  fi

  if [ $mode = 'dev' ]; then # Development-only codes

    echo "Mode = development"
    echo "Checking out required components..."

    ${ME_INSTALL}/checkout_externals > $TOPDIR/install_checkout.log 2>&1
    rc=$?
    if [[ $rc -ne 0 ]] ; then
      echo "FATAL CHECKOUT ERROR: Please check the install_checkout.log for details!"
      echo "Exiting..."
      exit $rc
    else
      echo "Checkout complete."
    fi

  elif [ $mode = 'prod' ]; then # Also includes optional codes for ops

    echo "Mode = production"
    echo "Checking out required and optional components..."

    ${ME_INSTALL}/checkout_externals --optional > $TOPDIR/install_checkout.log 2>&1
    rc=$?
    if [[ $rc -ne 0 ]] ; then
      echo "FATAL CHECKOUT ERROR: Please check the $TOPDIR/install_checkout.log for details!"
      echo "Exiting..."
      exit $rc
    else
      echo "Checkout complete."
    fi

    # Move gtg code for build
    cd $TOPDIR/sorc/gfs_post.fd
    cp sorc/post_gtg.fd/*f90  sorc/ncep_post.fd/.

  fi #dev/prod mode

fi # CHECKOUT=YES

#------------------------------------
# RUN BUILD
#------------------------------------
if [ $BUILD = "YES" ]; then

  echo "Building components..."

  cd $TOPDIR/sorc
  #sh build_all.sh > $TOPDIR/install_build.log 2>&1
  sh build_all.sh
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "FATAL BUILD ERROR: Please check the $TOPDIR/install_build.log for details!"
    echo "Exiting..."
    exit $rc
  else
    echo "Build complete."
  fi

fi # BUILD=YES

#------------------------------------
# RUN LINKING
#------------------------------------
if [ $LINK = "YES" ]; then

  echo "Linking components..."

  cd $TOPDIR/sorc
  sh link_fv3gfs.sh $mode $target > $TOPDIR/install_link.log 2>&1
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "FATAL LINKING ERROR: Please check the $TOPDIR/install_link.log for details!"
    echo "Exiting..."
    exit $rc
  else
    echo "Linking complete."
  fi

fi # LINK=YES

#-----------------------------------------------------
if [ $CHECKOUT = "YES" -a $BUILD = "YES" -a $LINK = "YES" ]; then
  echo;echo " .... Installation finished .... "
fi
exit 0
