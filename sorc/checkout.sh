#!/bin/sh
#set -xue
set -x

while getopts "oc" option; do
 case $option in
  o)
   echo "Received -o flag for optional checkout of GTG, will check out GTG with EMC_post"
   checkout_gtg="YES"
   ;;
  c)
   echo "Received -c flag, running coupled model" 
   COUPLED="YES"
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

echo ufs-weather-model checkout ...
if [ ${COUPLED:-"NO"} = "NO" ]; then
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
else 
  if [[ ! -d ufs_coupled.fd ]] ; then
    git clone https://github.com/ufs-community/ufs-weather-model ufs_coupled.fd >> ${topdir}/checkout-ufs_coupled.log 2>&1
    cd ufs_coupled.fd
    git checkout 9bbb6d466368d46797e2c72a9d08955fda39cf33 
    git submodule update --init --recursive
    cd ${topdir} 
  else
    echo 'Skip.  Directory ufs_coupled.fd already exists.'
  fi 
fi

echo gsi checkout ...
if [[ ! -d gsi.fd ]] ; then
    rm -f ${topdir}/checkout-gsi.log
    git clone --recursive https://github.com/NOAA-EMC/GSI.git gsi.fd >> ${topdir}/checkout-gsi.log 2>&1
    cd gsi.fd
    git checkout gfsda.v16.1.0
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
    git checkout gldas_gfsv16_release.v1.13.0
    cd ${topdir}
else
    echo 'Skip.  Directory gldas.fd already exists.'
fi

echo ufs_utils checkout ...
if [[ ! -d ufs_utils.fd ]] ; then
    rm -f ${topdir}/checkout-ufs_utils.log
    git clone https://github.com/NOAA-EMC/UFS_UTILS.git ufs_utils.fd >> ${topdir}/checkout-ufs_utils.fd.log 2>&1
    cd ufs_utils.fd
    git checkout ops-gfsv16.0.0
    cd ${topdir}
else
    echo 'Skip.  Directory ufs_utils.fd already exists.'
fi

echo EMC_post checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_post.log
    git clone https://github.com/NOAA-EMC/EMC_post.git gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    cd gfs_post.fd
    git checkout upp_v10.0.3
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
      cp sorc/post_gtg.fd/*f90 sorc/ncep_post.fd/.
      cp sorc/post_gtg.fd/gtg.config.gfs parm/gtg.config.gfs
    fi
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_post.fd already exists.'
fi

echo EMC_gfs_wafs checkout ...
if [[ ! -d gfs_wafs.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_wafs.log
    git clone --recursive https://github.com/NOAA-EMC/EMC_gfs_wafs.git gfs_wafs.fd >> ${topdir}/checkout-gfs_wafs.log 2>&1
    cd gfs_wafs.fd
    git checkout gfs_wafs.v6.0.22
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_wafs.fd already exists.'
fi

echo EMC_verif-global checkout ...
if [[ ! -d verif-global.fd ]] ; then
    rm -f ${topdir}/checkout-verif-global.log
    git clone --recursive https://github.com/NOAA-EMC/EMC_verif-global.git verif-global.fd >> ${topdir}/checkout-verif-global.log 2>&1
    cd verif-global.fd
    git checkout verif_global_v2.0.2
    cd ${topdir}
else
    echo 'Skip. Directory verif-global.fd already exist.'
fi

exit 0
