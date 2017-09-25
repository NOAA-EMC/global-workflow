#! /bin/sh

set -xue

USERNAME=${1:-""}    # gerrit username
if [[ "$USERNAME" = "" ]] ; then
   echo specify valid gerrit username for gsi checkout
   exit 
fi

echo fv3gfs checkout...
if [[ ! -d fv3gfs.fd ]] ; then
    svn co -r96963 \
        https://svnemc.ncep.noaa.gov/projects/nems/apps/NEMSfv3gfs/branches/russ-old-fv3gfs \
        fv3gfs.fd > checkout-fv3gfs.log 2>&1
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo gsi checkout using $USERNAME ...
if [[ ! -d gsi.fd ]] ; then
    set +e
    git clone --recursive ${USERNAME}@gerrit:ProdGSI gsi.fd
    cd gsi.fd
    git clone ${USERNAME}@gerrit:GSI-fix fix
    cd fix
    git checkout rev2
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi
