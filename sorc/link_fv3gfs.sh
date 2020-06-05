#!/bin/bash
set -x

#--make symbolic links for EMC installation and hardcopies for NCO delivery
. ./machine-setup.sh
echo "target system (machine) set to $target"

RUN_ENVIR=${1}
machine=${2}
if [ $# -eq 3 ]; then
model=${3}
else
model="uncoupled"
fi

if [ $# -lt 2 ]; then
    echo '***ERROR*** must specify two arguements: (1) RUN_ENVIR, (2) machine'
    echo ' Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | theia | hera | orion )'
    exit 1
fi

RUN_ENVIR=${1:-emc}

if [ $RUN_ENVIR != emc -a $RUN_ENVIR != nco ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | theia | hera | orion )'
    exit 1
fi
if [ $machine != cray -a $machine != theia -a $machine != dell -a $machine != hera -a $machine != orion]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | theia | hera | orion )'
    exit 1
fi

LINK="ln -fs"
SLINK="ln -fs"
[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

#------------------------------
#--model fix fields
#------------------------------
echo "target: $target"
if [ $target == "wcoss_cray" ]; then
    FIX_DIR="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $target = "wcoss_dell_p3" ]; then
    FIX_DIR="/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $target == "theia" ]; then
    FIX_DIR="/scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix"
elif [ $target == "gaea" ]; then
    FIX_DIR="/lustre/f1/pdata/ncep_shared/FV3GFS_V1_RELEASE/fix"
    echo "gaea says what, FIX_DIR = $FIX_DIR"
elif [ $target == "jet" ]; then
    FIX_DIR="/lfs3/projects/hfv3gfs/glopara/git/fv3gfs/fix"
elif [ $target == "hera" ]; then
    FIX_DIR="/scratch2/NCEPDEV/climate/climpara/S2S/FIX/fix_UFSp4"
####    FIX_DIR="/scratch1/NCEPDEV/global/Lin.Gan/git/reg2grb2_fix/test/fix_UFSp4"
elif [ $target == "orion" ]; then
####    FIX_DIR="/work/noaa/marine/jmeixner/tempFixICdir/fix/fix_prep_benchmark3"
    FIX_DIR="/work/noaa/marine/jmeixner/tempFixICdir/fix_UFSp4"
else
    echo 'CRITICAL: links to fix files not set'
    [[ $machine != orion ]] && exit 1
fi

if [ ! -d ${pwd}/../fix ]; then mkdir ${pwd}/../fix; fi
cd ${pwd}/../fix                ||exit 8

for dir in fix_am fix_fv3 fix_orog fix_fv3_gmted2010 fix_verif ; do
    [[ -d $dir ]] && rm -rf $dir
done
$LINK $FIX_DIR/* .

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

#---------------------------------------
#--add files from external repositories
#---------------------------------------
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
    $LINK ../sorc/ufs_utils.fd/scripts/exemcsfc_global_sfc_prep.sh.ecf .
if [ $model = "coupled" ]; then
    $LINK exgfs_nceppost_cpl.sh.ecf exgfs_nceppost.sh.ecf
fi
cd ${pwd}/../ush                ||exit 8
    for file in fv3gfs_downstream_nems.sh  fv3gfs_dwn_nems.sh  gfs_nceppost.sh  \
        gfs_transfer.sh  link_crtm_fix.sh  trim_rh.sh fix_precip.sh; do
        $LINK ../sorc/gfs_post.fd/ush/$file                  .
    done
    for file in emcsfc_ice_blend.sh  fv3gfs_driver_grid.sh  fv3gfs_make_orog.sh  global_cycle_driver.sh \
        emcsfc_snow.sh  fv3gfs_filter_topo.sh  global_chgres_driver.sh  global_cycle.sh \
        fv3gfs_chgres.sh  fv3gfs_make_grid.sh  global_chgres.sh ; do
        $LINK ../sorc/ufs_utils.fd/ush/$file                  .
    done

if [ $model = "coupled" ]; then
    $LINK fv3gfs_downstream_nems_cpl.sh fv3gfs_downstream_nems.sh
fi

cd ${pwd}/../util               ||exit 8
    for file in sub_slurm sub_wcoss_c sub_wcoss_d ; do
        $LINK ../sorc/ufs_utils.fd/util/$file
    done

#------------------------------
#--add gfs_wafs link if on Dell
if [ $machine = dell -o $machine = hera ]; then 
#------------------------------
 cd ${pwd}/../jobs               ||exit 8
     $LINK ../sorc/gfs_wafs.fd/jobs/*                         .
 cd ${pwd}/../parm               ||exit 8
     [[ -d wafs ]] && rm -rf wafs
    $LINK ../sorc/gfs_wafs.fd/parm/wafs                      wafs
 cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gfs_wafs.fd/scripts/*                      .
 cd ${pwd}/../ush                ||exit 8
    $LINK ../sorc/gfs_wafs.fd/ush/*                          .
 cd ${pwd}/../fix                ||exit 8
    $LINK ../sorc/gfs_wafs.fd/fix/*                          .
fi

#------------------------------
#--add GSI/EnKF file
#------------------------------
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

#------------------------------
#--add DA Monitor file (NOTE: ensure to use correct version)
#------------------------------
cd ${pwd}/../fix                ||exit 8
    [[ -d gdas ]] && rm -rf gdas
    mkdir -p gdas
    cd gdas
    $LINK ../../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gdas.v1.0.0/fix/gdas_minmon_cost.txt            .
    $LINK ../../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gdas.v1.0.0/fix/gdas_minmon_gnorm.txt           .
    $LINK ../../sorc/gsi.fd/util/Ozone_Monitor/nwprod/gdas_oznmon.v2.0.0/fix/gdas_oznmon_base.tar            .
    $LINK ../../sorc/gsi.fd/util/Ozone_Monitor/nwprod/gdas_oznmon.v2.0.0/fix/gdas_oznmon_satype.txt          .
    $LINK ../../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/fix/gdas_radmon_base.tar         .
    $LINK ../../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/fix/gdas_radmon_satype.txt       .
    $LINK ../../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/fix/gdas_radmon_scaninfo.txt     .
cd ${pwd}/../jobs               ||exit 8
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gdas.v1.0.0/jobs/JGDAS_VMINMON                     .
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gfs.v1.0.0/jobs/JGFS_VMINMON                       .
    $LINK ../sorc/gsi.fd/util/Ozone_Monitor/nwprod/gdas_oznmon.v2.0.0/jobs/JGDAS_VERFOZN                     .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/jobs/JGDAS_VERFRAD                  .
cd ${pwd}/../parm               ||exit 8
    [[ -d mon ]] && rm -rf mon
    mkdir -p mon
    cd mon
    $LINK ../../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/parm/gdas_radmon.parm            da_mon.parm
#   $LINK ../../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gdas.v1.0.0/parm/gdas_minmon.parm               .
#   $LINK ../../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gfs.v1.0.0/parm/gfs_minmon.parm                 .
    $LINK ../../sorc/gsi.fd/util/Ozone_Monitor/nwprod/gdas_oznmon.v2.0.0/parm/gdas_oznmon.parm               .
#   $LINK ../../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/parm/gdas_radmon.parm            .
cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gdas.v1.0.0/scripts/exgdas_vrfminmon.sh.ecf        .
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gfs.v1.0.0/scripts/exgfs_vrfminmon.sh.ecf          .
    $LINK ../sorc/gsi.fd/util/Ozone_Monitor/nwprod/gdas_oznmon.v2.0.0/scripts/exgdas_vrfyozn.sh.ecf          .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/scripts/exgdas_vrfyrad.sh.ecf       .
cd ${pwd}/../ush                ||exit 8
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/minmon_shared.v1.0.1/ush/minmon_xtrct_costs.pl     .
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/minmon_shared.v1.0.1/ush/minmon_xtrct_gnorms.pl    .
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/minmon_shared.v1.0.1/ush/minmon_xtrct_reduct.pl    .
    $LINK ../sorc/gsi.fd/util/Ozone_Monitor/nwprod/oznmon_shared.v2.0.0/ush/ozn_xtrct.sh                     .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/ush/radmon_ck_stdout.sh           .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/ush/radmon_err_rpt.sh             .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/ush/radmon_verf_angle.sh          .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/ush/radmon_verf_bcoef.sh          .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/ush/radmon_verf_bcor.sh           .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/ush/radmon_verf_time.sh           .
    
#------------------------------
#--link executables 
#------------------------------

cd $pwd/../exec
if [ $model == "coupled" ]; then
[[ -s nems_fv3_mom6_cice5.x ]] && rm -f nems_fv3_mom6_cice5.x
$LINK ../sorc/fv3_coupled.fd/NEMS/exe/nems_fv3_mom6_cice5.x .
else
[[ -s global_fv3gfs.x ]] && rm -f global_fv3gfs.x
$LINK ../sorc/fv3gfs.fd/NEMS/exe/global_fv3gfs.x .
fi

[[ -s gfs_ncep_post ]] && rm -f gfs_ncep_post
$LINK ../sorc/gfs_post.fd/exec/ncep_post gfs_ncep_post

if [[ $target == "jet" ]]; then
  util_exec_dir_path=/mnt/lfs3/projects/hfv3gfs/glopara/git/fv3gfs_builds
  #for util_exec_dirs in grib_utils prod_util gsi_tJet ;do
  for util_exec_dirs in grib_utils prod_util ;do
      if [[ -d ${util_exec_dir_path}/${util_exec_dirs} ]]; then
       $LINK ${util_exec_dir_path}/${util_exec_dirs}/* .
      else
       echo "WARNING ${util_exec} did not copy softlink from ${util_exec_dir_path} on Jet"
      fi
  done 
fi

#if [ $machine = dell ]; then 
#    for wafsexe in wafs_awc_wafavn  wafs_blending  wafs_cnvgrib2  wafs_gcip  wafs_makewafs  wafs_setmissing; do
#        [[ -s $wafsexe ]] && rm -f $wafsexe
#        $LINK ../sorc/gfs_wafs.fd/exec/$wafsexe .
#    done
#fi
if [ $machine = dell -o $machine = hera ]; then 
    for wafsexe in wafs_awc_wafavn  wafs_blending  wafs_cnvgrib2  wafs_gcip  wafs_makewafs  wafs_setmissing; do
        [[ -s $wafsexe ]] && rm -f $wafsexe
        $LINK ../sorc/gfs_wafs.fd/exec/$wafsexe .
    done
fi

for ufs_utilsexe in \
     chgres_cube.exe   fregrid           make_hgrid           nemsio_get    shave.x \
     emcsfc_ice_blend  fregrid_parallel  make_hgrid_parallel  nemsio_read \
     emcsfc_snow2mdl   global_chgres     make_solo_mosaic     nst_tf_chg.x \
     filter_topo       global_cycle      mkgfsnemsioctl       orog.x ; do
    [[ -s $ufs_utilsexe ]] && rm -f $ufs_utilsexe
    $LINK ../sorc/ufs_utils.fd/exec/$ufs_utilsexe .
done

for gsiexe in  global_gsi.x global_enkf.x calc_increment_ens.x  getsfcensmeanp.x  getsigensmeanp_smooth.x  \
    getsigensstatp.x  nc_diag_cat_serial.x nc_diag_cat.x recentersigp.x oznmon_horiz.x oznmon_time.x \
    radmon_angle.x radmon_bcoef.x radmon_bcor.x radmon_time.x ;do
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
  util_exec_dir_path=/mnt/lfs3/projects/hfv3gfs/glopara/git/fv3gfs_builds
  #for util_exec_dirs in grib_utils prod_util gsi_tJet ;do
  for util_exec_dirs in grib_utils prod_util ;do
      if [[ -d ${util_exec_dir_path}/${util_exec_dirs} ]]; then
       $LINK ${util_exec_dir_path}/${util_exec_dirs}/* .
      else
       echo "WARNING ${util_exec} did not copy softlink from ${util_exec_dir_path} on Jet"
      fi
  done 
fi
if [ $target = wcoss_dell_p3 ]; then
    for wafsexe in wafs_awc_wafavn  wafs_blending  wafs_cnvgrib2  wafs_gcip  wafs_makewafs  wafs_setmissing; do
        [[ -s $wafsexe ]] && rm -f $wafsexe
        $LINK ../sorc/gfs_wafs.fd/exec/$wafsexe .
    done
fi
 
#cd ${pwd}
#cd gsi.fd
#gsi_branch=`git branch | grep \*`
#cd ../fv3gfs.fd
#fv3gfs_branch=`git branch | grep \*`
#cd ../gfs_post.fd
#gfspost_branch=`git branch | grep \*`

#set +x
#echo "FV3  Branch: $fv3gfs_branch"
#echo "GSI  Branch: $gsi_branch"
#echo "POST Branch: $gfspost_branch"


#------------------------------
#--link source code directories
#------------------------------

cd ${pwd}/../sorc   ||   exit 8
    $SLINK gsi.fd/util/EnKF/gfs/src/calc_increment_ens.fd                                  calc_increment_ens.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/getsfcensmeanp.fd                                      getsfcensmeanp.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/getsigensmeanp_smooth.fd                               getsigensmeanp_smooth.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/getsigensstatp.fd                                      getsigensstatp.fd
    $SLINK gsi.fd/src                                                                      global_enkf.fd
    $SLINK gsi.fd/src                                                                      global_gsi.fd
    $SLINK gsi.fd/util/Ozone_Monitor/nwprod/oznmon_shared.v2.0.0/sorc/oznmon_horiz.fd      oznmon_horiz.fd
    $SLINK gsi.fd/util/Ozone_Monitor/nwprod/oznmon_shared.v2.0.0/sorc/oznmon_time.fd       oznmon_time.fd
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radang.fd    radmon_angle.fd
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radbcoef.fd  radmon_bcoef.fd
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radbcor.fd   radmon_bcor.fd 
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radtime.fd   radmon_time.fd 
    $SLINK gsi.fd/util/EnKF/gfs/src/recentersigp.fd                                        recentersigp.fd

    $SLINK gfs_post.fd/sorc/ncep_post.fd                                                   gfs_ncep_post.fd

    $SLINK ufs_utils.fd/sorc/fre-nctools.fd/tools/shave.fd                                 shave.fd
    for prog in filter_topo fregrid make_hgrid make_solo_mosaic ; do
        $SLINK ufs_utils.fd/sorc/fre-nctools.fd/tools/$prog                                ${prog}.fd                                
    done
    for prog in  chgres_cube.fd       global_cycle.fd   nemsio_read.fd \
                 emcsfc_ice_blend.fd  mkgfsnemsioctl.fd  nst_tf_chg.fd \
                 emcsfc_snow2mdl.fd   global_chgres.fd  nemsio_get.fd      orog.fd ;do
        $SLINK ufs_utils.fd/sorc/$prog                                                     $prog
    done


    if [ $machine = dell -o $machine = hera ]; then
        $SLINK gfs_wafs.fd/sorc/wafs_awc_wafavn.fd                                              wafs_awc_wafavn.fd
        $SLINK gfs_wafs.fd/sorc/wafs_blending.fd                                                wafs_blending.fd
        $SLINK gfs_wafs.fd/sorc/wafs_cnvgrib2.fd                                                wafs_cnvgrib2.fd
        $SLINK gfs_wafs.fd/sorc/wafs_gcip.fd                                                    wafs_gcip.fd
        $SLINK gfs_wafs.fd/sorc/wafs_makewafs.fd                                                wafs_makewafs.fd
        $SLINK gfs_wafs.fd/sorc/wafs_setmissing.fd                                              wafs_setmissing.fd
    fi


#------------------------------
#--choose dynamic config.base for EMC installation 
#--choose static config.base for NCO installation 
cd $pwd/../parm/config
[[ -s config.base ]] && rm -f config.base 
if [ $RUN_ENVIR = nco ] ; then
 cp -p config.base.nco.static config.base
else
 cp -p config.base.emc.dyn config.base
fi
#------------------------------
if [ $model = "coupled" ] ; then
 rm -f config.base
 cp -p config.base.emc.dyn_coupled config.base
 if [ $machine = "theia" ] ; then
 CPLFIX_DIR="/scratch4/NCEPDEV/nems/save/Bin.Li/fix_prep_benchmark2"
 elif [ $machine = "hera" ] ; then
 CPLFIX_DIR="/scratch2/NCEPDEV/climate/Bin.Li/S2S/fix/fix_prep_benchmark3"
 fi
cd $pwd/../fix
# Add fixed files needed for coupled fv3-mom6-cice5
#$LINK $CPLFIX_DIR/fix_fv3   .
#$LINK $CPLFIX_DIR/fix_fv3_gmted2010   .
$LINK $CPLFIX_DIR/fix_ocnice   .
$LINK $CPLFIX_DIR/fix_cice5    .
$LINK $CPLFIX_DIR/fix_mom6     .
$LINK $CPLFIX_DIR/fix_fv3grid  .
fi

exit 0
