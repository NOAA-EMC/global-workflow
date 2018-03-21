#!/bin/ksh
set -ex

#--make symbolic links for EMC installation and hardcopies for NCO delivery
. ./machine-setup.sh
echo "target system (machine) set to $target"

if [ -z $target ]; then
    echo 'target value not set (unknown system not supported)'
    exit 1
fi

RUN_ENVIR=${1:-emc}

if [ $RUN_ENVIR != emc -a $RUN_ENVIR != nco ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc )'
    exit 1
fi
if [ $target != wcoss_cray -a $target != theia -a $target != gaea -a $target != jet ]; then
    echo '$target value set to unknown or unsupported system'
    exit 1
fi

LINK="ln -fs"
[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

#--model fix fields
if [ $target == "wcoss_cray" ]; then
    FIX_DIR="/gpfs/hps3/emc/global/noscrub/emc.glopara/FV3GFS_V1_RELEASE/fix"
elif [ $target = "theia" ]; then
    FIX_DIR="/scratch4/NCEPDEV/global/noscrub/glopara/FV3GFS_V1_RELEASE/fix"
elif [ $target = "gaea" ]; then
    FIX_DIR="/lustre/f1/pdata/ncep_shared/FV3GFS_V1_RELEASE/fix"
elif [ $target = "jet" ]; then
    FIX_DIR="/lfs3/projects/hfv3gfs/Samuel.Trahan/fix-fv3gfs"
else
    echo 'CRITICAL: links to fix files not set'
    exit 1
fi

if [ ! -r $FIX_DIR ]; then
   echo "CRITICAL: you do not of read permissions to the location of the fix file $FIX_DIR"
   exit -1
fi

if [ ! -z $FIX_DIR ]; then
 if [ ! -d ${pwd}/../fix ]; then mkdir ${pwd}/../fix; fi
 cd ${pwd}/../fix                ||exit 8
 for dir in fix_am fix_fv3 fix_orog fix_fv3_gmted2010 ; do
     [[ -d $dir ]] && rm -rf $dir
 done
 $LINK $FIX_DIR/* .
fi


#--add gfs_post file
#cd ${pwd}/../jobs               ||exit 8
#    $LINK ../sorc/gfs_post.fd/jobs/JGLOBAL_NCEPPOST          .
cd ${pwd}/../parm               ||exit 8
    [[ -d post ]] && rm -rf post
    $LINK ../sorc/gfs_post.fd/parm                           post
#cd ${pwd}/../scripts            ||exit 8
#    $LINK ../sorc/gfs_post.fd/scripts/exgdas_nceppost.sh.ecf .
#    $LINK ../sorc/gfs_post.fd/scripts/exgfs_nceppost.sh.ecf  .
cd ${pwd}/../ush                ||exit 8
    $LINK ../sorc/gfs_post.fd/ush/gfs_transfer.sh .
#    for file in gfs_nceppost.sh gfs_transfer.sh; do
#        $LINK ../sorc/gfs_post.fd/ush/$file                  .
#    done


#--link executables 

cd $pwd/../exec

if [[ -f ../sorc/fv3gfs.fd/NEMS/exe/fv3_gfs_nh.prod.32bit.x ]]; then
 [[ -s fv3_gfs_nh.prod.32bit.x ]] && rm -f fv3_gfs_nh.prod.32bit.x
 $LINK ../sorc/fv3gfs.fd/NEMS/exe/fv3_gfs_nh.prod.32bit.x .
else
  echo 'WARNING fv3_gfs_nh.prod.32bit.x executable file does not exsist, no link to fv3_gfs_nh.prod.32bit.x made'
fi

if [[ -f ../sorc/gfs_post.fd/exec/ncep_post ]]; then
 [[ -s gfs_ncep_post ]] && rm -f gfs_ncep_post
 $LINK ../sorc/gfs_post.fd/exec/ncep_post gfs_ncep_post
else
  echo 'WARNING ncep_post executable file does not exsist, no link to ncep_post made'
fi
 
exit 0
