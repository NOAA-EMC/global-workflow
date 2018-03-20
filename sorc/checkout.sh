#!/bin/sh

set -xue

USERNAME=${1:-$USER} # gerrit username

set +e

topdir=$(pwd)
echo $topdir

echo fv3gfs checkout ...
if [[ ! -d fv3gfs.fd ]] ; then
    rm -f ${topdir}/checkout-fv3gfs.log
    git clone --recursive ${USERNAME}@gerrit:NEMSfv3gfs fv3gfs.fd >> ${topdir}/checkout-fv3gfs.log 2>&1
    cd fv3gfs.fd
	#git checkout nemsfv3gfs_beta_v1.0.0
    git checkout rusty_46546
    git submodule update --init --recursive
    cd ${topdir}
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo EMC_post checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_post.log
    git clone --recursive ${USERNAME}@gerrit:EMC_post gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    cd gfs_post.fd
    git checkout ncep_post.v8.0.2
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_post.fd already exists.'
fi

# add temp fix to build post with Georeg's updtes
cp post_add_temp/build_ncep_post.sh gfs_post.fd/sorc
cp post_add_temp/v7.0.0-gaea  gfs_post.fd/modulefiles/post
cp post_add_temp/module-setup.sh.inc fv3gfs.fd/NEMS/src/conf

exit 0
