#!/bin/sh
#set -xue
set -x

while getopts "o" option;
do
 case $option in
  o)
   echo "Received -o flag for optional checkout of operational-only codes"
   checkout_gtg="YES"
   checkout_wafs="YES"
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

topdir=$(pwd)
echo $topdir

echo fv3gfs checkout ...
if [[ ! -d fv3gfs.fd ]] ; then
    rm -f ${topdir}/checkout-fv3gfs.log
    git clone https://github.com/ufs-community/ufs-weather-model fv3gfs.fd >> ${topdir}/checkout-fv3gfs.log 2>&1
    cd fv3gfs.fd
    git checkout 9350745855aebe0790813e0ed2ba5ad680e3f75c
    git submodule update --init --recursive
    cd ${topdir}
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo gsi checkout ...
if [[ ! -d gsi.fd ]] ; then
    rm -f ${topdir}/checkout-gsi.log
#    git clone --recursive https://github.com/NOAA-EMC/GSI.git gsi.fd >> ${topdir}/checkout-gsi.log 2>&1
    git clone --recursive git@github.com:AndrewEichmann-NOAA/GSI.git gsi.fd >> ${topdir}/checkout-gsi.log 2>&1
    cd gsi.fd
    git checkout EXP-efso_fv3
#    git checkout 9c1fc15d42573b398037319bbf8d5143ad126fb6
    git submodule update
    cd ${topdir}
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi

echo gldas checkout ...
if [[ ! -d gldas.fd ]] ; then
    rm -f ${topdir}/checkout-gldas.log
    git clone https://github.com/NOAA-EMC/GLDAS.git gldas.fd >> ${topdir}/checkout-gldas.fd.log 2>&1
    cd gldas.fd
    git checkout gldas_gfsv16_release.v1.15.0
    cd ${topdir}
else
    echo 'Skip.  Directory gldas.fd already exists.'
fi

echo ufs_utils checkout ...
if [[ ! -d ufs_utils.fd ]] ; then
    rm -f ${topdir}/checkout-ufs_utils.log
    git clone --recursive https://github.com/NOAA-EMC/UFS_UTILS.git ufs_utils.fd >> ${topdir}/checkout-ufs_utils.fd.log 2>&1
    cd ufs_utils.fd
    git checkout ufs_utils_1_6_0
    cd ${topdir}
else
    echo 'Skip.  Directory ufs_utils.fd already exists.'
fi

echo UPP checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_post.log
    git clone https://github.com/NOAA-EMC/UPP.git gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    cd gfs_post.fd
    git checkout ff42e0227d6100285d4179a2572b700fd5a959cb
    git submodule update --init CMakeModules

    ################################################################################
    # checkout_gtg
    ## yes: The gtg code at NCAR private repository is available for ops. GFS only.
    #       Only approved persons/groups have access permission.
    ## no:  No need to check out gtg code for general GFS users.
    ################################################################################
    checkout_gtg=${checkout_gtg:-"NO"}
    if [[ ${checkout_gtg} == "YES" ]] ; then
      ./manage_externals/checkout_externals
      cp sorc/post_gtg.fd/*F90 sorc/ncep_post.fd/.
      cp sorc/post_gtg.fd/gtg.config.gfs parm/gtg.config.gfs
    fi
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_post.fd already exists.'
fi

checkout_wafs=${checkout_wafs:-"NO"}
if [[ ${checkout_wafs} == "YES" ]] ; then
  echo EMC_gfs_wafs checkout ...
  if [[ ! -d gfs_wafs.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_wafs.log
    git clone --recursive https://github.com/NOAA-EMC/EMC_gfs_wafs.git gfs_wafs.fd >> ${topdir}/checkout-gfs_wafs.log 2>&1
    cd gfs_wafs.fd
    git checkout c2a29a67d9432b4d6fba99eac7797b81d05202b6
    cd ${topdir}
  else
    echo 'Skip.  Directory gfs_wafs.fd already exists.'
  fi
fi

echo EMC_verif-global checkout ...
if [[ ! -d verif-global.fd ]] ; then
    rm -f ${topdir}/checkout-verif-global.log
    git clone --recursive https://github.com/NOAA-EMC/EMC_verif-global.git verif-global.fd >> ${topdir}/checkout-verif-global.log 2>&1
    cd verif-global.fd
    git checkout verif_global_v2.8.0
    cd ${topdir}
else
    echo 'Skip. Directory verif-global.fd already exist.'
fi

exit 0
