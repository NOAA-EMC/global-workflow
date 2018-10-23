#!/bin/ksh
set -ex

#--make symbolic links for EMC installation and hardcopies for NCO delivery

RUN_ENVIR=${1}
machine=${2}

if [ $# -lt 2 ]; then
    echo '***ERROR*** must specify two arguements: (1) RUN_ENVIR, (2) machine'
    echo ' Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | theia )'
    exit 1
fi

if [ $RUN_ENVIR != emc -a $RUN_ENVIR != nco ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | theia )'
    exit 1
fi
if [ $machine != cray -a $machine != theia -a $machine != dell ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | theia )'
    exit 1
fi

LINK="ln -fs"
SLINK="ln -fs"
[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

#------------------------------
#--model fix fields
#------------------------------
if [ $machine == "cray" ]; then
    FIX_DIR="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "dell" ]; then
    FIX_DIR="/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "theia" ]; then
    FIX_DIR="/scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix"
fi
cd ${pwd}/../fix                ||exit 8
for dir in fix_am fix_fv3 fix_orog fix_fv3_gmted2010 ; do
    [[ -d $dir ]] && rm -rf $dir
done
$LINK $FIX_DIR/* .


#------------------------------
#--add gfs_post file
#------------------------------
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


#------------------------------
#--add gfs_wafs file if on Dell
#if [ $machine = dell ]; then 
#------------------------------
#cd ${pwd}/../jobs               ||exit 8
#    $LINK ../sorc/gfs_wafs.fd/jobs/*                         .
#cd ${pwd}/../parm               ||exit 8
#    [[ -d wafs ]] && rm -rf wafs
#    $LINK ../sorc/gfs_wafs.fd/parm/wafs                      wafs
#cd ${pwd}/../scripts            ||exit 8
#    $LINK ../sorc/gfs_wafs.fd/scripts/*                      .
#cd ${pwd}/../ush                ||exit 8
#    $LINK ../sorc/gfs_wafs.fd/ush/*                          .
#cd ${pwd}/../fix                ||exit 8
#    $LINK ../sorc/gfs_wafs.fd/fix/*                          .
#fi


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
[[ -s global_fv3gfs.x ]] && rm -f global_fv3gfs.x
$LINK ../sorc/fv3gfs.fd/NEMS/exe/global_fv3gfs.x .

[[ -s gfs_ncep_post ]] && rm -f gfs_ncep_post
$LINK ../sorc/gfs_post.fd/exec/ncep_post gfs_ncep_post

#if [ $machine = dell ]; then 
#    for wafsexe in wafs_awc_wafavn  wafs_blending  wafs_cnvgrib2  wafs_gcip  wafs_makewafs  wafs_setmissing; do
#        [[ -s $wafsexe ]] && rm -f $wafsexe
#        $LINK ../sorc/gfs_wafs.fd/exec/$wafsexe .
#    done
#fi


for gsiexe in  global_gsi.x global_enkf.x calc_increment_ens.x  getsfcensmeanp.x  getsigensmeanp_smooth.x  getsigensstatp.x  recentersigp.x oznmon_horiz.x oznmon_time.x radmon_angle radmon_bcoef radmon_bcor radmon_time ;do
    [[ -s $gsiexe ]] && rm -f $gsiexe
    $LINK ../sorc/gsi.fd/exec/$gsiexe .
done


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


exit 0



