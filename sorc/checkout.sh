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
    git checkout gefs_v12_retro2
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
    git checkout fv3da.v1.0.43 >> ${LOG_DIR}/checkout-gsi.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    git submodule update
    rc=$?
    ((err+=$rc))
    # Workaround for missing GSI file on lowres parallel runs
    cd fix/Big_Endian/
    git checkout ae3bc2538f34c7cdb6a533a14c4880f6970a6724^ global_berror.l64y98.f77  >> ${LOG_DIR}/checkout-gsi.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    git checkout ae3bc2538f34c7cdb6a533a14c4880f6970a6724^ global_berror.l64y194.f77  >> ${LOG_DIR}/checkout-gsi.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    # Check out historical ozinfo files for use in retrospective parallels
    cd ${topdir}/gsi.fd/fix/fv3_historical
    git checkout cd0847ee5c67115113f61c79e7d8bc6b1b7095ba  0readme.ozinfo  >> ${LOG_DIR}/checkout-gsi.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    git checkout cd0847ee5c67115113f61c79e7d8bc6b1b7095ba  global_ozinfo.txt.2015110500  >> ${LOG_DIR}/checkout-gsi.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    git checkout cd0847ee5c67115113f61c79e7d8bc6b1b7095ba  global_ozinfo.txt.2018110700  >> ${LOG_DIR}/checkout-gsi.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    cd ${topdir}
else
    echo 'Skip.  Directory gsi.fd already exists.'
fi

echo ufs_utils checkout ...
if [[ ! -d ufs_utils.fd ]] ; then
    rm -f ${LOG_DIR}/checkout-ufs_utils.log
    git clone --recursive gerrit:UFS_UTILS ufs_utils.fd >> ${LOG_DIR}/checkout-ufs_utils.fd.log 2>&1
    rc=$?
    ((err+=$rc))
    cd ufs_utils.fd
    # git checkout v1.0.0
    git checkout e7eb676
    cd ${topdir}
else
    echo 'Skip.  Directory ufs_utils.fd already exists.'
fi

echo EMC_post checkout ...
if [[ ! -d gfs_post.fd ]] ; then
    rm -f ${LOG_DIR}/checkout-gfs_post.log
    git clone --recursive gerrit:EMC_post gfs_post.fd >> ${LOG_DIR}/checkout-gfs_post.log 2>&1
    rc=$?
    ((err+=$rc))
    #git clone --recursive gerrit:EMC_post_gtg gfs_post.fd >> ${topdir}/checkout-gfs_post.log 2>&1
    cd gfs_post.fd
    # git checkout ncep_post.v8.0.27e
    git checkout 88e936c
    #git checkout ncep_post_gtg.v1.0.6c
    cd ${topdir}
else
    echo 'Skip.  Directory gfs_post.fd already exists.'
fi

echo EMC_gfs_wafs checkout ...
if [[ ! -d gfs_wafs.fd ]] ; then
   rm -f ${topdir}/checkout-gfs_wafs.log
   git clone --recursive gerrit:EMC_gfs_wafs gfs_wafs.fd >> ${topdir}/checkout-gfs_wafs.log 2>&1
   cd gfs_wafs.fd
   git checkout gfs_wafs.v5.0.8
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
    cd ${topdir}
else
    echo 'Skip.  Directory gsd_prep_chem.fd already exists.'
fi

#### Exception handling
[[ $err -ne 0 ]] && echo "FATAL CHECKOUT ERROR: Please check checkout-*.log file under checkout directory for detail, ABORT!"
$ERRSCRIPT || exit $err

exit 0
