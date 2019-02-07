#!/bin/sh
set -xu

topdir=$(pwd)
echo $topdir

echo fv3gfs checkout ...
if [[ ! -d fv3gfs.fd ]] ; then
    rm -f ${topdir}/checkout-fv3gfs.log
    git clone --recursive gerrit:NEMSfv3gfs fv3gfs.fd >> ${topdir}/checkout-fv3gfs.log 2>&1
    cd fv3gfs.fd
    git checkout nemsfv3gfs_beta_v1.0.12
    git submodule update --init --recursive
    cd ${topdir}
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo gsi checkout ...
if [[ ! -d gsi.fd ]] ; then
    rm -f ${topdir}/checkout-gsi.log
    git clone --recursive gerrit:ProdGSI gsi.fd >> ${topdir}/checkout-gsi.fd.log 2>&1
    cd gsi.fd
    git checkout fv3da.v1.0.38
    git submodule update
    cd ${topdir}
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi

echo EMC_post checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_post.log
    git clone --recursive gerrit:EMC_post gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    #git clone --recursive gerrit:EMC_post_gtg gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    cd gfs_post.fd
    git checkout ncep_post.v8.0.27c
    #git checkout ncep_post_gtg.v1.0.6a
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_post.fd already exists.'
fi

#echo EMC_gfs_wafs checkout ...
#if [[ ! -d gfs_wafs.fd ]] ; then
#    rm -f ${topdir}/checkout-gfs_wafs.log
#    git clone --recursive gerrit:EMC_gfs_wafs gfs_wafs.fd >> ${topdir}/checkout-gfs_wafs.log 2>&1
#    cd gfs_wafs.fd
#    git checkout gfs_wafs.v5.0.7
#    cd ${topdir}
#else
#    echo 'Skip.  Directory gfs_wafs.fd already exists.'
#fi

exit 0
