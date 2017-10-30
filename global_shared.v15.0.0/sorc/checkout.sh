#!/bin/sh

set -xue

USERNAME=${1:-$USER} # gerrit username

set +e

echo fv3gfs checkout ...
if [[ ! -d fv3gfs.fd ]] ; then
    rm -f checkout-fv3gfs.log
    git clone --recursive \
        ssh://${USERNAME}@vlab.ncep.noaa.gov:29418/NEMSfv3gfs.git \
        fv3gfs.fd > checkout-fv3gfs.log 2>&1
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo gsi checkout ...
if [[ ! -d gsi.fd ]] ; then
    rm -f checkout-gsi.log
    git clone --recursive \
        ssh://${USERNAME}@vlab.ncep.noaa.gov:29418/ProdGSI.git \
        gsi.fd > checkout-gsi.fd.log 2>&1
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi

exit 0
