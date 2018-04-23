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
    FIX_DIR="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $target == "theia" ]; then
    FIX_DIR="/scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix"
elif [ $target == "gaea" ]; then
    FIX_DIR="/lustre/f1/pdata/ncep_shared/FV3GFS_V1_RELEASE/fix"
elif [ $target == "jet" ]; then
    FIX_DIR="/lfs3/projects/hfv3gfs/glopara/git/fv3gfs/fix"
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
cd ${pwd}/../jobs               ||exit 8
    $LINK ../sorc/gfs_post.fd/jobs/JGLOBAL_POST_MANAGER      .
    $LINK ../sorc/gfs_post.fd/jobs/JGLOBAL_NCEPPOST          .
cd ${pwd}/../parm               ||exit 8
    [[ -d post ]] && rm -rf post
    $LINK ../sorc/gfs_post.fd/parm                           post
cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gfs_post.fd/scripts/exgdas_nceppost.sh.ecf .
    $LINK ../sorc/gfs_post.fd/scripts/exgfs_nceppost.sh.ecf  .
    $LINK ../sorc/gfs_post.fd/scripts/exglobal_pmgr.sh.ecf   .
cd ${pwd}/../ush                ||exit 8
    for file in fv3gfs_downstream_nems.sh  fv3gfs_dwn_nems.sh  gfs_nceppost.sh  gfs_transfer.sh  link_crtm_fix.sh  trim_rh.sh fix_precip.sh; do
        $LINK ../sorc/gfs_post.fd/ush/$file                  .
    done

#--add GSI/EnKF file
cd ${pwd}/../jobs               ||exit 8
    $LINK ../sorc/gsi.fd/jobs/JGLOBAL_ANALYSIS           .
    $LINK ../sorc/gsi.fd/jobs/JGLOBAL_ENKF_SELECT_OBS    .
    $LINK ../sorc/gsi.fd/jobs/JGLOBAL_ENKF_INNOVATE_OBS  .
    $LINK ../sorc/gsi.fd/jobs/JGLOBAL_ENKF_UPDATE        .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_RECENTER        .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_FCST            .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_POST            .
cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gsi.fd/scripts/exglobal_analysis_fv3gfs.sh.ecf           .
    $LINK ../sorc/gsi.fd/scripts/exglobal_innovate_obs_fv3gfs.sh.ecf       .
    $LINK ../sorc/gsi.fd/scripts/exglobal_enkf_innovate_obs_fv3gfs.sh.ecf  .
    $LINK ../sorc/gsi.fd/scripts/exglobal_enkf_update_fv3gfs.sh.ecf        .
    $LINK ../sorc/gsi.fd/scripts/exglobal_enkf_recenter_fv3gfs.sh.ecf      .
    $LINK ../sorc/gsi.fd/scripts/exglobal_enkf_fcst_fv3gfs.sh.ecf          .
    $LINK ../sorc/gsi.fd/scripts/exglobal_enkf_post_fv3gfs.sh.ecf          .
cd ${pwd}/../fix                ||exit 8
    [[ -d fix_gsi ]] && rm -rf fix_gsi
    $LINK ../sorc/gsi.fd/fix  fix_gsi

#--link executables 

cd $pwd/../exec

[[ -s fv3_gfs_nh.prod.32bit.x ]] && rm -f fv3_gfs_nh.prod.32bit.x
$LINK ../sorc/fv3gfs.fd/NEMS/exe/fv3_gfs_nh.prod.32bit.x .

[[ -s gfs_ncep_post ]] && rm -f gfs_ncep_post
$LINK ../sorc/gfs_post.fd/exec/ncep_post gfs_ncep_post

for gsiexe in  global_gsi global_enkf calc_increment_ens.x  getsfcensmeanp.x  getsigensmeanp_smooth.x  getsigensstatp.x  recentersigp.x ;do
    [[ -s $gsiexe ]] && rm -f $gsiexe
    $LINK ../sorc/gsi.fd/exec/$gsiexe .
done

if [[ $target == "gaea" ]]; then
  if [[ -f /lustre/f1/pdata/ncep_shared/exec/wgrib2 ]]; then
   cp /lustre/f1/pdata/ncep_shared/exec/wgrib2 .
  else
   echo 'WARNING wgrib2 did not copy from /lustre/f1/pdata/ncep_shared/exec on Gaea'
  fi
fi
if [[ $target == "jet" ]]; then
  if [[ -f /mnt/lfs3/projects/hfv3gfs/gwv/fv3/exec/wgrib2 ]]; then
   cp /mnt/lfs3/projects/hfv3gfs/gwv/fv3/exec/wgrib2 .
  else
   echo 'WARNING wgrib2 did not copy from /mnt/lfs3/projects/hfv3gfs/gwv/fv3/exec on Jet'
  fi
fi

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
