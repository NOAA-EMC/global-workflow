#!/bin/sh
set -xue

topdir=$(pwd)
echo $topdir

if [ $# -eq 1 ]; then
model=$1
fi
model=${model:-"uncoupled"}

if [ $model = "coupled" ]; then
echo fv3_mom6_cice5 checkout ...
rm -f ${topdir}/checkout-fv3_coupled.log

if [[ ! -d fv3_coupled.fd ]] ; then
    git clone https://github.com/ufs-community/ufs-s2s-model fv3_coupled.fd >> ${topdir}/checkout-fv3_coupled.log 2>&1
    cd fv3_coupled.fd
    git checkout s2s_prototype4.0
    git submodule update --init --recursive
    cd ${topdir}
else
    echo 'Skip.  Directory fv3_coupled.fd already exists.'
fi
fi

if [ $model != "coupled" ]; then
echo fv3gfs checkout ...
if [[ ! -d fv3gfs.fd ]] ; then
    rm -f ${topdir}/checkout-fv3gfs.log
    git clone https://github.com/ufs-community/ufs-weather-model fv3gfs.fd >> ${topdir}/checkout-fv3gfs.log 2>&1
    cd fv3gfs.fd
    git checkout GFS.v16.0.1
    git submodule update --init --recursive
    cd ${topdir}
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi
fi

if [ $model != "coupled" ]; then
    echo gsi checkout ...
    if [[ ! -d gsi.fd ]] ; then
        rm -f ${topdir}/checkout-gsi.log
        git clone --recursive gerrit:ProdGSI gsi.fd >> ${topdir}/checkout-gsi.log 2>&1
        cd gsi.fd
#       git checkout gfsda.v16.0.0
        git checkout feature/parallel_ncio
        git submodule update
        cd ${topdir}
    else
        echo 'Skip.  Directory gsi.fd already exists.'
    fi
fi

echo gldas checkout ...
if [[ ! -d gldas.fd ]] ; then
    rm -f ${topdir}/checkout-gldas.log
    git clone https://github.com/NOAA-EMC/GLDAS  gldas.fd >> ${topdir}/checkout-gldas.fd.log 2>&1
    cd gldas.fd
    git checkout gldas_gfsv16_release.v1.0.0
    cd ${topdir}
else
    echo 'Skip.  Directory gldas.fd already exists.'
fi

if [ $model != "coupled" ]; then
    echo ufs_utils checkout ...
    if [[ ! -d ufs_utils.fd ]] ; then
        rm -f ${topdir}/checkout-ufs_utils.log
        git clone https://github.com/NOAA-EMC/UFS_UTILS.git ufs_utils.fd >> ${topdir}/checkout-ufs_utils.fd.log 2>&1
        cd ufs_utils.fd
        git checkout release/ops-gfsv16 
        cd ${topdir}
    else
        echo 'Skip.  Directory ufs_utils.fd already exists.'
    fi
fi

echo EMC_post checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_post.log
    git clone https://github.com/NOAA-EMC/EMC_post.git gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    cd gfs_post.fd
    git checkout upp_gfsv16_release.v1.0.8
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_post.fd already exists.'
fi

if [ $model != "coupled" ]; then
    echo EMC_gfs_wafs checkout ...
    if [[ ! -d gfs_wafs.fd ]] ; then
        rm -f ${topdir}/checkout-gfs_wafs.log
        git clone --recursive https://github.com/NOAA-EMC/EMC_gfs_wafs.git gfs_wafs.fd >> ${topdir}/checkout-gfs_wafs.log 2>&1
        cd gfs_wafs.fd
        git checkout gfs_wafs.v5.0.11
        cd ${topdir}
    else
        echo 'Skip.  Directory gfs_wafs.fd already exists.'
    fi
fi

if [ $model != "coupled" ]; then
    echo EMC_verif-global checkout ...
    if [[ ! -d verif-global.fd ]] ; then
        rm -f ${topdir}/checkout-verif-global.log
        git clone --recursive https://github.com/NOAA-EMC/EMC_verif-global.git verif-global.fd >> ${topdir}/checkout-verif-global.log 2>&1
        cd verif-global.fd
        git checkout verif_global_v1.6.0
        cd ${topdir}
    else
        echo 'Skip. Directory verif-global.fd already exist.'
    fi
fi

exit 0
