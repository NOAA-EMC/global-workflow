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
    echo 'Skip checout.  Directory fv3gfs.fd already exists.'
fi

echo EMC_post checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_post.log
    git clone --recursive ${USERNAME}@gerrit:EMC_post gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    cd gfs_post.fd
    git checkout ncep_post.v8.0.2
    cd ${topdir}
else
    echo 'Skip checkout. Directory gfs_post.fd already exists.'
fi

# add temp fix to build post and fcst with George's updtes
if [[ -d  gfs_post.fd ]] ; then
 cp add_post_temp/build_ncep_post.sh gfs_post.fd/sorc
 cp add_post_temp/v7.0.0-gaea  gfs_post.fd/modulefiles/post
 cp ../modulefiles/module-setup.sh.inc fv3gfs.fd/NEMS/src/conf
else
 echo 'WARNING directory gfs_post.fd does not exsist so build updates were not moved from post_add_temp'
fi

if [[ -d  fv3gfs.fd ]] ; then
 cp add_fv3gfs_temp/configure.fv3.jet fv3gfs.fd/conf
 cp add_fv3gfs_temp/fv3 fv3gfs.fd/modulefiles/jet
else
 echo 'WARNING directory fv3gfs.fd does not exsist so build updates were not mved from fv3gfs_add_temp'
fi

exit 0
