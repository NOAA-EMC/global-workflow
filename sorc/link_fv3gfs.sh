#!/bin/ksh
set -ex

#--make symbolic links for EMC installation and hardcopies for NCO delivery

RUN_ENVIR=${1}
machine=${2}
if [ $# -eq 3 ]; then
  model=${3}
else
  model="uncoupled"
fi

if [ $# -lt 2 ]; then
    echo '***ERROR*** must specify two arguements: (1) RUN_ENVIR, (2) machine'
    echo ' Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | hera | orion )'
    echo ' A third argument is needed when coupled: '
    echo ' Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | hera | orion ) coupled'
    exit 1
fi

if [ $RUN_ENVIR != emc -a $RUN_ENVIR != nco ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | hera | orion )'
    echo ' A third argument is needed when coupled: '
    echo ' Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | hera | orion ) coupled'
    exit 1
fi
if [ $machine != cray -a $machine != dell -a $machine != hera -a $machine != orion ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | hera | orion )'
    echo ' A third argument is needed when coupled: '
    echo ' Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | dell | hera | orion ) coupled'
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
    FIX_DIR="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix_nco_gfsv16"
elif [ $machine = "dell" ]; then
    FIX_DIR="/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix_nco_gfsv16"
elif [ $machine = "hera" ]; then
    if [ $model = "coupled" ]; then
       FIX_DIR="/scratch2/NCEPDEV/climate/climpara/S2S/FIX/fix_UFSp6"
    else
       FIX_DIR="/scratch1/NCEPDEV/global/glopara/fix_nco_gfsv16"
    fi
elif [ $machine = "orion" ]; then
    if [ $model = "coupled" ]; then
       FIX_DIR="/work/noaa/marine/jmeixner/tempFixICdir/fix_UFSp6"
    else
       FIX_DIR="/work/noaa/global/glopara/fix_nco_gfsv16"
    fi
fi

if [ ! -z $FIX_DIR ]; then
 if [ ! -d ${pwd}/../fix ]; then mkdir ${pwd}/../fix; fi
fi
cd ${pwd}/../fix                ||exit 8
if [ $model = "coupled" ] ; then
  for dir in fix_am fix_fv3_gmted2010 fix_gldas fix_orog fix_verif fix_cice fix_mom6 fix_cpl fix_wave fix_reg2grb2 ; do 
    if [ -d $dir ]; then
      [[ $RUN_ENVIR = nco ]] && chmod -R 755 $dir
      rm -rf $dir
    fi
    $LINK $FIX_DIR/$dir .
  done
else 
  for dir in fix_am fix_fv3_gmted2010 fix_gldas fix_orog fix_verif fix_wave_gfs ; do
    if [ -d $dir ]; then
      [[ $RUN_ENVIR = nco ]] && chmod -R 755 $dir
      rm -rf $dir
    fi
    $LINK $FIX_DIR/$dir .
  done
fi

if [ -d ${pwd}/ufs_utils.fd ]; then
  cd ${pwd}/ufs_utils.fd/sorc
  ./link_fixdirs.sh $RUN_ENVIR $machine
fi

#---------------------------------------
#--add files from external repositories
#---------------------------------------
cd ${pwd}/../jobs               ||exit 8
    $LINK ../sorc/gfs_post.fd/jobs/JGLOBAL_ATMOS_POST_MANAGER      .
    $LINK ../sorc/gfs_post.fd/jobs/JGLOBAL_ATMOS_NCEPPOST          .
    $LINK ../sorc/gldas.fd/jobs/JGDAS_ATMOS_GLDAS            .             
cd ${pwd}/../parm               ||exit 8
    [[ -d post ]] && rm -rf post
    $LINK ../sorc/gfs_post.fd/parm                           post
    [[ -d gldas ]] && rm -rf gldas
    $LINK ../sorc/gldas.fd/parm                              gldas
cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gfs_post.fd/scripts/exgdas_atmos_nceppost.sh .
    if [ $model = "coupled" ]; then
      $LINK exgfs_nceppost_cpl.sh.ecf exgfs_nceppost.sh.ecf
    else 
      $LINK ../sorc/gfs_post.fd/scripts/exgfs_atmos_nceppost.sh  .
    fi 
    $LINK ../sorc/gfs_post.fd/scripts/exglobal_atmos_pmgr.sh   .
    $LINK ../sorc/ufs_utils.fd/scripts/exemcsfc_global_sfc_prep.sh .
    $LINK ../sorc/gldas.fd/scripts/exgdas_atmos_gldas.sh .
cd ${pwd}/../ush                ||exit 8
    for file in fv3gfs_downstream_nems.sh fv3gfs_dwn_nems.sh gfs_nceppost.sh  \
        gfs_transfer.sh mod_icec.sh link_crtm_fix.sh trim_rh.sh fix_precip.sh; do
        $LINK ../sorc/gfs_post.fd/ush/$file                  .
    done
    if [ $model = "coupled" ]; then
       rm fv3gfs_downstream_nems.sh
       $LINK fv3gfs_downstream_nems_cpl.sh fv3gfs_downstream_nems.sh
    fi
    for file in emcsfc_ice_blend.sh  fv3gfs_driver_grid.sh  fv3gfs_make_orog.sh  global_cycle_driver.sh \
        emcsfc_snow.sh  fv3gfs_filter_topo.sh  global_chgres_driver.sh  global_cycle.sh \
        fv3gfs_chgres.sh  fv3gfs_make_grid.sh  global_chgres.sh  ; do
        $LINK ../sorc/ufs_utils.fd/ush/$file                  .
    done
    for file in gldas_archive.sh  gldas_forcing.sh gldas_get_data.sh  gldas_process_data.sh gldas_liscrd.sh  gldas_post.sh ; do
        $LINK ../sorc/gldas.fd/ush/$file                  .
    done
cd ${pwd}/../util               ||exit 8
    for file in sub_slurm sub_wcoss_c sub_wcoss_d ; do
        $LINK ../sorc/ufs_utils.fd/util/$file .
    done


#-----------------------------------
#--add gfs_wafs link if checked out
if [ -d ${pwd}/gfs_wafs.fd ]; then 
#-----------------------------------
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
    [[ -d wafs ]] && rm -rf wafs
    $LINK ../sorc/gfs_wafs.fd/fix/*                          .
fi


#------------------------------
#--add GSI/EnKF file
#------------------------------
cd ${pwd}/../jobs               ||exit 8
    $LINK ../sorc/gsi.fd/jobs/JGLOBAL_ATMOS_ANALYSIS        .
    $LINK ../sorc/gsi.fd/jobs/JGLOBAL_ATMOS_ANALYSIS_CALC   .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ATMOS_ANALYSIS_DIAG     .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_SELECT_OBS         .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_DIAG               .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_UPDATE             .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_ECEN               .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_SFC                .    
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_FCST               .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ENKF_POST               .
    $LINK ../sorc/gsi.fd/jobs/JGDAS_ATMOS_CHGRES_FORENKF    .
cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gsi.fd/scripts/exglobal_atmos_analysis.sh       .
    $LINK ../sorc/gsi.fd/scripts/exglobal_atmos_analysis_calc.sh  .
    $LINK ../sorc/gsi.fd/scripts/exglobal_diag.sh                 .
    $LINK ../sorc/gsi.fd/scripts/exgdas_enkf_select_obs.sh        .
    $LINK ../sorc/gsi.fd/scripts/exgdas_enkf_update.sh            .
    $LINK ../sorc/gsi.fd/scripts/exgdas_enkf_ecen.sh              .
    $LINK ../sorc/gsi.fd/scripts/exgdas_enkf_sfc.sh               .
    $LINK ../sorc/gsi.fd/scripts/exgdas_enkf_fcst.sh              .
    $LINK ../sorc/gsi.fd/scripts/exgdas_enkf_post.sh              .
    $LINK ../sorc/gsi.fd/scripts/exgdas_atmos_chgres_forenkf.sh   .
cd ${pwd}/../fix                ||exit 8
    [[ -d fix_gsi ]] && rm -rf fix_gsi
    $LINK ../sorc/gsi.fd/fix  fix_gsi
cd ${pwd}/../ush                ||exit 8
    $LINK ../sorc/gsi.fd/ush/gsi_utils.py        .
    $LINK ../sorc/gsi.fd/ush/calcanl_gfs.py      .
    $LINK ../sorc/gsi.fd/ush/calcinc_gfs.py      .
    $LINK ../sorc/gsi.fd/ush/getncdimlen         .


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
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gdas.v1.0.0/jobs/JGDAS_ATMOS_VMINMON               .
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gfs.v1.0.0/jobs/JGFS_ATMOS_VMINMON                 .
    $LINK ../sorc/gsi.fd/util/Ozone_Monitor/nwprod/gdas_oznmon.v2.0.0/jobs/JGDAS_ATMOS_VERFOZN               .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/jobs/JGDAS_ATMOS_VERFRAD            .
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
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gdas.v1.0.0/scripts/exgdas_atmos_vminmon.sh        .
    $LINK ../sorc/gsi.fd/util/Minimization_Monitor/nwprod/gfs.v1.0.0/scripts/exgfs_atmos_vminmon.sh          .
    $LINK ../sorc/gsi.fd/util/Ozone_Monitor/nwprod/gdas_oznmon.v2.0.0/scripts/exgdas_atmos_verfozn.sh        .
    $LINK ../sorc/gsi.fd/util/Radiance_Monitor/nwprod/gdas_radmon.v3.0.0/scripts/exgdas_atmos_verfrad.sh     .
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
if  [ $model == "coupled" ]; then
 [[ -s ufs_model ]] && rm -f ufs_model
  $LINK ../sorc/ufs_coupled.fd/build/ufs_model . 
else
[[ -s global_fv3gfs.x ]] && rm -f global_fv3gfs.x
$LINK ../sorc/fv3gfs.fd/NEMS/exe/global_fv3gfs.x .
if [ -d ../sorc/fv3gfs.fd/WW3/exec ]; then # Wave execs
  for waveexe in ww3_gint ww3_grib ww3_grid ww3_multi ww3_ounf ww3_ounp ww3_outf ww3_outp ww3_prep ww3_prnc; do
    [[ -s $waveexe ]] && rm -f $waveexe
    $LINK ../sorc/fv3gfs.fd/WW3/exec/$waveexe .
  done
fi
fi 

[[ -s gfs_ncep_post ]] && rm -f gfs_ncep_post
$LINK ../sorc/gfs_post.fd/exec/ncep_post gfs_ncep_post

if [ -d ${pwd}/gfs_wafs.fd ]; then 
    for wafsexe in \
          wafs_awc_wafavn  wafs_blending  wafs_blending_0p25 \
          wafs_cnvgrib2  wafs_gcip  wafs_grib2_0p25 \
          wafs_makewafs  wafs_setmissing; do
        [[ -s $wafsexe ]] && rm -f $wafsexe
        $LINK ../sorc/gfs_wafs.fd/exec/$wafsexe .
    done
fi

for ufs_utilsexe in \
     emcsfc_ice_blend  emcsfc_snow2mdl  global_chgres  global_cycle ; do
    [[ -s $ufs_utilsexe ]] && rm -f $ufs_utilsexe
    $LINK ../sorc/ufs_utils.fd/exec/$ufs_utilsexe .
done

for gsiexe in  calc_analysis.x calc_increment_ens_ncio.x calc_increment_ens.x \
    getsfcensmeanp.x getsigensmeanp_smooth.x getsigensstatp.x global_enkf.x global_gsi.x \
    interp_inc.x ncdiag_cat.x oznmon_horiz.x oznmon_time.x radmon_angle.x \
    radmon_bcoef.x radmon_bcor.x radmon_time.x recentersigp.x;do
    [[ -s $gsiexe ]] && rm -f $gsiexe
    $LINK ../sorc/gsi.fd/exec/$gsiexe .
done

for gldasexe in gdas2gldas  gldas2gdas  gldas_forcing  gldas_model  gldas_post  gldas_rst; do
    [[ -s $gldasexe ]] && rm -f $gldasexe
    $LINK ../sorc/gldas.fd/exec/$gldasexe .
done

#------------------------------
#--link source code directories
#------------------------------

cd ${pwd}/../sorc   ||   exit 8
    [[ -d calc_analysis.fd ]] && rm -rf calc_analysis.fd
    $SLINK gsi.fd/util/netcdf_io/calc_analysis.fd                                          calc_analysis.fd

    [[ -d calc_increment_ens.fd ]] && rm -rf calc_increment_ens.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/calc_increment_ens.fd                                  calc_increment_ens.fd

    [[ -d calc_increment_ens_ncio.fd ]] && rm -rf calc_increment_ens_ncio.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/calc_increment_ens_ncio.fd                             calc_increment_ens_ncio.fd

    [[ -d getsfcensmeanp.fd ]] && rm -rf getsfcensmeanp.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/getsfcensmeanp.fd                                      getsfcensmeanp.fd

    [[ -d getsigensmeanp_smooth.fd ]] && rm -rf getsigensmeanp_smooth.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/getsigensmeanp_smooth.fd                               getsigensmeanp_smooth.fd

    [[ -d getsigensstatp.fd ]] && rm -rf getsigensstatp.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/getsigensstatp.fd                                      getsigensstatp.fd

    [[ -d global_enkf.fd ]] && rm -rf global_enkf.fd
    $SLINK gsi.fd/src/enkf                                                                 global_enkf.fd

    [[ -d global_gsi.fd ]] && rm -rf global_gsi.fd
    $SLINK gsi.fd/src/gsi                                                                  global_gsi.fd

    [[ -d interp_inc.fd ]] && rm -rf interp_inc.fd
    $SLINK gsi.fd/util/netcdf_io/interp_inc.fd                                             interp_inc.fd

    [[ -d ncdiag.fd ]] && rm -rf ncdiag.fd
    $SLINK gsi.fd/src/ncdiag                                                               ncdiag_cat.fd

    [[ -d oznmon_horiz.fd ]] && rm -rf oznmon_horiz.fd
    $SLINK gsi.fd/util/Ozone_Monitor/nwprod/oznmon_shared.v2.0.0/sorc/oznmon_horiz.fd      oznmon_horiz.fd

    [[ -d oznmon_time.fd ]] && rm -rf oznmon_time.fd
    $SLINK gsi.fd/util/Ozone_Monitor/nwprod/oznmon_shared.v2.0.0/sorc/oznmon_time.fd       oznmon_time.fd

    [[ -d radmon_angle.fd ]] && rm -rf radmon_angle.fd
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radang.fd    radmon_angle.fd

    [[ -d radmon_bcoef.fd ]] && rm -rf radmon_bcoef.fd
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radbcoef.fd  radmon_bcoef.fd

    [[ -d radmon_bcor.fd ]] && rm -rf radmon_bcor.fd
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radbcor.fd   radmon_bcor.fd 

    [[ -d radmon_time.fd ]] && rm -rf radmon_time.fd
    $SLINK gsi.fd/util/Radiance_Monitor/nwprod/radmon_shared.v3.0.0/sorc/verf_radtime.fd   radmon_time.fd 

    [[ -d recentersigp.fd ]] && rm -rf recentersigp.fd
    $SLINK gsi.fd/util/EnKF/gfs/src/recentersigp.fd                                        recentersigp.fd

    $SLINK gfs_post.fd/sorc/ncep_post.fd                                                   gfs_ncep_post.fd

    $SLINK ufs_utils.fd/sorc/fre-nctools.fd/tools/shave.fd                                 shave.fd
    for prog in filter_topo fregrid make_hgrid make_solo_mosaic ; do
        $SLINK ufs_utils.fd/sorc/fre-nctools.fd/tools/$prog                                ${prog}.fd                                
    done
    for prog in  global_cycle.fd   nemsio_read.fd  nemsio_chgdate.fd \
        emcsfc_ice_blend.fd  nst_tf_chg.fd \
        emcsfc_snow2mdl.fd   global_chgres.fd  nemsio_get.fd    orog.fd ;do
        $SLINK ufs_utils.fd/sorc/$prog                                                     $prog
    done


    if [ -d ${pwd}/gfs_wafs.fd ]; then 
        $SLINK gfs_wafs.fd/sorc/wafs_awc_wafavn.fd                                              wafs_awc_wafavn.fd
        $SLINK gfs_wafs.fd/sorc/wafs_blending.fd                                                wafs_blending.fd
        $SLINK gfs_wafs.fd/sorc/wafs_cnvgrib2.fd                                                wafs_cnvgrib2.fd
        $SLINK gfs_wafs.fd/sorc/wafs_gcip.fd                                                    wafs_gcip.fd
        $SLINK gfs_wafs.fd/sorc/wafs_makewafs.fd                                                wafs_makewafs.fd
        $SLINK gfs_wafs.fd/sorc/wafs_setmissing.fd                                              wafs_setmissing.fd
    fi

    for prog in gdas2gldas.fd  gldas2gdas.fd  gldas_forcing.fd  gldas_model.fd  gldas_post.fd  gldas_rst.fd ;do
        $SLINK gldas.fd/sorc/$prog                                                     $prog
    done

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

