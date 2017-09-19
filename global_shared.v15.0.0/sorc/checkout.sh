#! /bin/sh

set -xue

echo fv3gfs checkout...
if [[ ! -d fv3gfs.fd ]] ; then
    svn co -r96963 \
        https://svnemc.ncep.noaa.gov/projects/nems/apps/NEMSfv3gfs/branches/russ-old-fv3gfs \
        fv3gfs.fd > checkout-fv3gfs.log 2>&1
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo gsi checkout...
if [[ ! -d gsi.fd ]] ; then
    svn co -r96892 --ignore-externals \
        https://svnemc.ncep.noaa.gov/projects/gsi/trunk \
        gsi.fd > checkout-gsi.log 2>&1
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi
