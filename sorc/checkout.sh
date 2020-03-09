#!/bin/sh
set -xu

# Checkout fv3gfs.fd gsi.fd ufs_utils.fd gfs_post.fd
# Arg "-F" indicate force of remove fv3gfs.fd gsi.fd ufs_utils.fd gfs_post.fd and checkout log directory 

if [ $# -lt 1 ]; then
  FORCE_REPLACE="N"
else
  FORCE_REPLACE=${1}
fi

[[ $FORCE_REPLACE == "-F" ]] && FORCE_REPLACE="Y"

topdir=$(pwd)
echo $topdir
LOG_DIR=$topdir/checkout
if [ -d $LOG_DIR -a $FORCE_REPLACE == "Y" ]; then
  rm -rf $topdir/fv3gfs.fd $topdir/gsi.fd $topdir/ufs_utils.fd $topdir/gfs_post.fd $topdir/gsd_prep_chem.fd $LOG_DIR
fi
mkdir -p $LOG_DIR

ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
err=0

echo fv3gfs checkout ...
if [[ ! -d fv3gfs.fd ]] ; then
    rm -f ${topdir}/checkout-fv3gfs.log
    git clone --recursive gerrit:EMC_FV3-GSDCHEM-WW3 fv3gfs.fd >> ${topdir}/checkout-fv3gfs.log 2>&1
    rc=$?
    ((err+=$rc))
    cd fv3gfs.fd
	git checkout 343d54d
	git submodule update --init --recursive
    rc=$?
    ((err+=$rc))
    cd ${topdir}
else
    echo 'Skip.  Directory fv3gfs.fd already exists.'
fi

echo gsi checkout ...
if [[ ! -d gsi.fd ]] ; then
    rm -f ${LOG_DIR}/checkout-gsi.log
    git clone --recursive gerrit:ProdGSI gsi.fd >> ${LOG_DIR}/checkout-gsi.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    cd gsi.fd
    git checkout d47a995ae
    git submodule update
    cd ${topdir}
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi

echo ufs_utils checkout ...
if [[ ! -d ufs_utils.fd ]] ; then
    rm -f ${topdir}/checkout-ufs_utils.log
    git clone https://github.com/NOAA-EMC/UFS_UTILS.git ufs_utils.fd >> ${topdir}/checkout-ufs_utils.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    cd ufs_utils.fd
    git checkout 9de34ea
    cd ${topdir}
else
    echo 'Skip.  Directory ufs_utils.fd already exists.'
fi

echo EMC_post checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_post.log
    git clone --recursive https://github.com/NOAA-EMC/EMC_post.git gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    rc=$?
    ((err+=$rc))
    cd gfs_post.fd
    git checkout 8d5deea
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_post.fd already exists.'
fi

echo EMC_gfs_wafs checkout ...
if [[ ! -d gfs_wafs.fd ]] ; then
    rm -f ${topdir}/checkout-gfs_wafs.log
    git clone --recursive https://github.com/NOAA-EMC/EMC_gfs_wafs.git gfs_wafs.fd >> ${topdir}/checkout-gfs_wafs.log 2>&1
    rc=$?
    ((err+=$rc))
    cd gfs_wafs.fd
    git checkout gfs_wafs.v5.0.11
    cd ${topdir}
else
   echo 'Skip.  Directory gfs_wafs.fd already exists.'
fi

echo GSD-prep-chem checkout ...
if [[ ! -d gsd_prep_chem.fd ]] ; then
    rm -f ${LOG_DIR}/checkout-gsd-prep-chem.log
    git clone gerrit:GSD-prep-chem gsd_prep_chem.fd >> ${LOG_DIR}/checkout-gsd-prep-chem.log 2>&1
    rc=$?
    ((err+=$rc))
    cd gsd_prep_chem.fd
    git checkout 9e200f7
    cd ${topdir}
else
    echo 'Skip.  Directory gsd_prep_chem.fd already exists.'
fi

echo EMC_verif-global checkout ...
if [[ ! -d verif-global.fd ]] ; then
    rm -f ${topdir}/checkout-verif-global.log
    git clone --recursive gerrit:EMC_verif-global verif-global.fd >> ${topdir}/checkout-verif-global.log 2>&1
    rc=$?
    ((err+=$rc))
    cd verif-global.fd
    git checkout verif_global_v1.2.2
    cd ${topdir}
else
    echo 'Skip. Directory verif-global.fd already exist.'
fi

#### Exception handling
[[ $err -ne 0 ]] && echo "FATAL CHECKOUT ERROR: Please check checkout-*.log file under checkout directory for detail, ABORT!"
$ERRSCRIPT || exit $err

exit 0
