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
    git clone --recursive ${USERNAME}@gerrit:FV3  FV3  >> ${topdir}/checkout-fv3gfs.log 2>&1    
    git clone --recursive ${USERNAME}@gerrit:NEMS NEMS >> ${topdir}/checkout-fv3gfs.log 2>&1
    cd ${topdir}
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo gsi checkout ...
if [[ ! -d gsi.fd ]] ; then
    rm -f ${topdir}/checkout-gsi.log
    git clone --recursive ${USERNAME}@gerrit:ProdGSI gsi.fd >> ${topdir}/checkout-gsi.fd.log 2>&1
    cd gsi.fd
    git clone --recursive ${USERNAME}@gerrit:GSI-fix fix >> ${topdir}/checkout-gsi.fd.log 2>&1
    cd fix
    git checkout rev2
    cd ${topdir}
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi

exit 0
