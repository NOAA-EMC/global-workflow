#!/bin/bash
set -ex

#--make symbolic links for EMC installation and hardcopies for NCO delivery

RUN_ENVIR=${1}
machine=${2}

if [ $# -lt 2 ]; then
    echo '***ERROR*** must specify two arguements: (1) RUN_ENVIR, (2) machine'
    echo ' Syntax: link_workflow.sh ( nco | emc ) ( hera | orion | jet | stampede )'
    exit 1
fi

if [ $RUN_ENVIR != emc -a $RUN_ENVIR != nco ]; then
    echo ' Syntax: link_workflow.sh ( nco | emc ) ( hera | orion | jet | stampede )'
    exit 1
fi
if [ $machine != hera -a $machine != orion -a $machine != jet -a $machine != stampede ]; then
    echo ' Syntax: link_workflow.sh ( nco | emc ) ( hera | orion | jet | stampede )'
    exit 1
fi

LINK="ln -fs"
SLINK="ln -fs"
[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

# Link post
[[ -d upp.fd ]] && rm -rf upp.fd
$LINK ufs_model.fd/FV3/upp upp.fd

#------------------------------
#--model fix fields
#------------------------------
if [ $machine = "hera" ]; then
    FIX_DIR="/scratch1/NCEPDEV/global/glopara/fix_NEW"
elif [ $machine = "orion" ]; then
    FIX_DIR="/work/noaa/global/glopara/fix_NEW"
elif [ $machine = "jet" ]; then
    FIX_DIR="/lfs4/HFIP/hfv3gfs/glopara/git/fv3gfs/fix_NEW"
elif [ $machine = "stampede" ]; then
    FIX_DIR="/work/07738/jkuang/stampede2/tempFixICdir/fix_UFSp6"
fi

if [ ! -z $FIX_DIR ]; then
 if [ ! -d ${pwd}/../fix ]; then mkdir ${pwd}/../fix; fi
fi
cd ${pwd}/../fix                ||exit 8
for dir in fix_aer \
            fix_am \
            fix_chem \
            fix_fv3_gmted2010 \
            fix_gldas \
            fix_lut \
            fix_fv3_fracoro \
            fix_orog \
            fix_sfc_climo \
            fix_verif \
            fix_cice \
            fix_mom6 \
            fix_cpl \
            fix_wave \
            fix_reg2grb2 \
            fix_ugwd
            do
    if [ -d $dir ]; then
      [[ $RUN_ENVIR = nco ]] && chmod -R 755 $dir
      rm -rf $dir
    fi
    $LINK $FIX_DIR/$dir .
done

if [ -d ${pwd}/ufs_utils.fd ]; then
  cd ${pwd}/ufs_utils.fd/fix
  ./link_fixdirs.sh $RUN_ENVIR $machine
fi


#---------------------------------------
#--add files from external repositories
#---------------------------------------
cd ${pwd}/../jobs               ||exit 8
if [ -d ../sorc/gldas.fd ]; then
    $LINK ../sorc/gldas.fd/jobs/JGDAS_ATMOS_GLDAS            .
fi
cd ${pwd}/../parm               ||exit 8
    # [[ -d post ]] && rm -rf post
    # $LINK ../sorc/upp.fd/parm                           post
    if [ -d ../sorc/gldas.fd ]; then
      [[ -d gldas ]] && rm -rf gldas
      $LINK ../sorc/gldas.fd/parm                         gldas
    fi
cd ${pwd}/../parm/post          ||exit 8
    for file in postxconfig-NT-GEFS-ANL.txt postxconfig-NT-GEFS-F00.txt postxconfig-NT-GEFS.txt postxconfig-NT-GFS-ANL.txt \
        postxconfig-NT-GFS-F00-TWO.txt postxconfig-NT-GFS-F00.txt postxconfig-NT-GFS-FLUX-F00.txt postxconfig-NT-GFS-FLUX.txt \
        postxconfig-NT-GFS-GOES.txt postxconfig-NT-GFS-TWO.txt postxconfig-NT-GFS-WAFS-ANL.txt postxconfig-NT-GFS-WAFS.txt \
        postxconfig-NT-GFS.txt postxconfig-NT-gefs-aerosol.txt postxconfig-NT-gefs-chem.txt params_grib2_tbl_new \
        post_tag_gfs128 post_tag_gfs65 gtg.config.gfs gtg_imprintings.txt nam_micro_lookup.dat \
        AEROSOL_LUTS.dat optics_luts_DUST.dat optics_luts_SALT.dat optics_luts_SOOT.dat optics_luts_SUSO.dat optics_luts_WASO.dat \
        ; do
        $LINK ../../sorc/upp.fd/parm/$file .
    done
cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/ufs_utils.fd/scripts/exemcsfc_global_sfc_prep.sh .
    if [ -d ../sorc/gldas.fd ]; then
      $LINK ../sorc/gldas.fd/scripts/exgdas_atmos_gldas.sh .
    fi
cd ${pwd}/../ush                ||exit 8
    for file in emcsfc_ice_blend.sh  fv3gfs_driver_grid.sh  fv3gfs_make_orog.sh  global_cycle_driver.sh \
        emcsfc_snow.sh  fv3gfs_filter_topo.sh  global_cycle.sh  fv3gfs_make_grid.sh ; do
        $LINK ../sorc/ufs_utils.fd/ush/$file                  .
    done
    if [ -d ../sorc/gldas.fd ]; then
      for file in gldas_archive.sh  gldas_forcing.sh gldas_get_data.sh  gldas_process_data.sh gldas_liscrd.sh  gldas_post.sh ; do
        $LINK ../sorc/gldas.fd/ush/$file                  .
      done
    fi


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
#--add GSI fix directory
#------------------------------
if [ -d ../sorc/gsi_enkf.fd ]; then
  cd ${pwd}/../fix                ||exit 8
    [[ -d fix_gsi ]] && rm -rf fix_gsi
    $LINK ../sorc/gsi_enkf.fd/fix  fix_gsi
fi

#------------------------------
#--add GDASApp fix directory
#------------------------------
if [ -d ../sorc/gdas.cd ]; then
  cd ${pwd}/../fix                ||exit 8
    [[ -d fix_gdas ]] && rm -rf fix_gdas
    $LINK $FIX_DIR/fix_gdas .
fi

#------------------------------
#--add GDASApp files
#------------------------------
if [ -d ../sorc/gdas.cd ]; then
  cd ${pwd}/../ush                ||exit 8
    $LINK ../sorc/gdas.cd/ush/ufsda                               .
fi


#------------------------------
#--add DA Monitor file (NOTE: ensure to use correct version)
#------------------------------
if [ -d ../sorc/gsi_monitor.fd ]; then

  cd ${pwd}/../fix                ||exit 8
    [[ -d gdas ]] && rm -rf gdas
    mkdir -p gdas
    cd gdas
    $LINK ../../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_cost.txt                   .
    $LINK ../../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_gnorm.txt                  .
    $LINK ../../sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_base.tar                   .
    $LINK ../../sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_satype.txt                 .
    $LINK ../../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_base.tar                .
    $LINK ../../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_satype.txt              .
    $LINK ../../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_scaninfo.txt            .
  cd ${pwd}/../jobs               ||exit 8
    $LINK ../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/jobs/JGDAS_ATMOS_VMINMON                      .
    $LINK ../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/jobs/JGFS_ATMOS_VMINMON                        .
    $LINK ../sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/jobs/JGDAS_ATMOS_VERFOZN                      .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/jobs/JGDAS_ATMOS_VERFRAD                   .
  cd ${pwd}/../parm               ||exit 8
    [[ -d mon ]] && rm -rf mon
    mkdir -p mon
    cd mon
    $LINK ../../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm                   da_mon.parm
    # $LINK ../../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/parm/gdas_minmon.parm                      .
    # $LINK ../../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/parm/gfs_minmon.parm                        .
    $LINK ../../sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/parm/gdas_oznmon.parm                      .
    # $LINK ../../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm                   .
  cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/scripts/exgdas_atmos_vminmon.sh               .
    $LINK ../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/scripts/exgfs_atmos_vminmon.sh                 .
    $LINK ../sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/scripts/exgdas_atmos_verfozn.sh               .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/scripts/exgdas_atmos_verfrad.sh            .
  cd ${pwd}/../ush                ||exit 8
    $LINK ../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/minmon_shared/ush/minmon_xtrct_costs.pl            .
    $LINK ../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/minmon_shared/ush/minmon_xtrct_gnorms.pl           .
    $LINK ../sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/minmon_shared/ush/minmon_xtrct_reduct.pl           .
    $LINK ../sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/ush/ozn_xtrct.sh                            .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/ush/radmon_err_rpt.sh                    .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/ush/radmon_verf_angle.sh                 .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/ush/radmon_verf_bcoef.sh                 .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/ush/radmon_verf_bcor.sh                  .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/ush/radmon_verf_time.sh                  .
    $LINK ../sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/ush/rstprod.sh                           .
fi

#------------------------------
#--link executables
#------------------------------

if [ ! -d $pwd/../exec ]; then mkdir $pwd/../exec ; fi
cd $pwd/../exec

[[ -s gaussian_sfcanl.exe ]] && rm -f gaussian_sfcanl.exe
$LINK ../sorc/install/bin/gaussian_sfcanl.x gaussian_sfcanl.exe
for workflowexec in fbwndgfs gfs_bufr regrid_nemsio supvit syndat_getjtbul \
    syndat_maksynrc syndat_qctropcy tocsbufr ; do
  [[ -s $workflowexec ]] && rm -f $workflowexec
  $LINK ../sorc/install/bin/${workflowexec}.x $workflowexec
done
for workflowexec in enkf_chgres_recenter.x enkf_chgres_recenter_nc.x fv3nc2nemsio.x \
    tave.x vint.x reg2grb2.x ; do
  [[ -s $workflowexec ]] && rm -f $workflowexec
  $LINK ../sorc/install/bin/$workflowexec .
done

[[ -s ufs_model.x ]] && rm -f ufs_model.x
$LINK ../sorc/ufs_model.fd/tests/ufs_model.x .

[[ -s gfs_ncep_post ]] && rm -f gfs_ncep_post
$LINK ../sorc/upp.fd/exec/upp.x gfs_ncep_post

if [ -d ${pwd}/gfs_wafs.fd ]; then
    for wafsexe in \
          wafs_awc_wafavn.x  wafs_blending.x  wafs_blending_0p25.x \
          wafs_cnvgrib2.x  wafs_gcip.x  wafs_grib2_0p25.x \
          wafs_makewafs.x  wafs_setmissing.x; do
        [[ -s $wafsexe ]] && rm -f $wafsexe
        $LINK ../sorc/gfs_wafs.fd/exec/$wafsexe .
    done
fi

for ufs_utilsexe in \
     emcsfc_ice_blend  emcsfc_snow2mdl  global_cycle ; do
    [[ -s $ufs_utilsexe ]] && rm -f $ufs_utilsexe
    $LINK ../sorc/ufs_utils.fd/exec/$ufs_utilsexe .
done

# GSI
if [ -d ../sorc/gsi_enkf.fd ]; then
  for exe in enkf.x gsi.x; do
    [[ -s $exe ]] && rm -f $exe
    $LINK ../sorc/gsi_enkf.fd/install/bin/$exe .
  done
fi

# GSI Utils
if [ -d ../sorc/gsi_utils.fd ]; then
  for exe in calc_analysis.x calc_increment_ens_ncio.x calc_increment_ens.x \
    getsfcensmeanp.x getsigensmeanp_smooth.x getsigensstatp.x \
    interp_inc.x recentersigp.x;do
    [[ -s $exe ]] && rm -f $exe
    $LINK ../sorc/gsi_utils.fd/install/bin/$exe .
  done
fi

# GSI Monitor
if [ -d ../sorc/gsi_monitor.fd ]; then
  for exe in oznmon_horiz.x oznmon_time.x radmon_angle.x \
    radmon_bcoef.x radmon_bcor.x radmon_time.x; do
    [[ -s $exe ]] && rm -f $exe
    $LINK ../sorc/gsi_monitor.fd/install/bin/$exe .
  done
fi

if [ -d ../sorc/gldas.fd ]; then
  for gldasexe in gdas2gldas  gldas2gdas  gldas_forcing  gldas_model  gldas_post  gldas_rst; do
    [[ -s $gldasexe ]] && rm -f $gldasexe
    $LINK ../sorc/gldas.fd/exec/$gldasexe .
  done
fi

# GDASApp
if [ -d ../sorc/gdas.cd ]; then
  for gdasexe in fv3jedi_addincrement.x fv3jedi_diffstates.x fv3jedi_ensvariance.x fv3jedi_hofx.x \
    fv3jedi_var.x fv3jedi_convertincrement.x fv3jedi_dirac.x fv3jedi_error_covariance_training.x \
    fv3jedi_letkf.x fv3jedi_convertstate.x fv3jedi_eda.x fv3jedi_forecast.x fv3jedi_plot_field.x \
    fv3jedi_data_checker.py fv3jedi_enshofx.x fv3jedi_hofx_nomodel.x fv3jedi_testdata_downloader.py; do
    [[ -s $gdasexe ]] && rm -f $gdasexe
    $LINK ../sorc/gdas.cd/build/bin/$gdasexe .
  done
fi

#------------------------------
#--link source code directories
#------------------------------
cd ${pwd}/../sorc   ||   exit 8

    if [ -d gsi_enkf.fd ]; then
      [[ -d gsi.fd ]] && rm -rf gsi.fd
      $SLINK gsi_enkf.fd/src/gsi                                                                gsi.fd

      [[ -d enkf.fd ]] && rm -rf enkf.fd
      $SLINK gsi_enkf.fd/src/enkf                                                               enkf.fd
    fi

    if [ -d gsi_utils.fd ]; then
      [[ -d calc_analysis.fd ]] && rm -rf calc_analysis.fd
      $SLINK gsi_utils.fd/src/netcdf_io/calc_analysis.fd                                        calc_analysis.fd

      [[ -d calc_increment_ens.fd ]] && rm -rf calc_increment_ens.fd
      $SLINK gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens.fd                                calc_increment_ens.fd

      [[ -d calc_increment_ens_ncio.fd ]] && rm -rf calc_increment_ens_ncio.fd
      $SLINK gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens_ncio.fd                           calc_increment_ens_ncio.fd

      [[ -d getsfcensmeanp.fd ]] && rm -rf getsfcensmeanp.fd
      $SLINK gsi_utils.fd/src/EnKF/gfs/src/getsfcensmeanp.fd                                    getsfcensmeanp.fd

      [[ -d getsigensmeanp_smooth.fd ]] && rm -rf getsigensmeanp_smooth.fd
      $SLINK gsi_utils.fd/src/EnKF/gfs/src/getsigensmeanp_smooth.fd                             getsigensmeanp_smooth.fd

      [[ -d getsigensstatp.fd ]] && rm -rf getsigensstatp.fd
      $SLINK gsi_utils.fd/src/EnKF/gfs/src/getsigensstatp.fd                                    getsigensstatp.fd

      [[ -d recentersigp.fd ]] && rm -rf recentersigp.fd
      $SLINK gsi_utils.fd/src/EnKF/gfs/src/recentersigp.fd                                      recentersigp.fd

      [[ -d interp_inc.fd ]] && rm -rf interp_inc.fd
      $SLINK gsi_utils.fd/src/netcdf_io/interp_inc.fd                                           interp_inc.fd
    fi

    if [ -d gsi_monitor.fd ] ; then
      [[ -d oznmon_horiz.fd ]] && rm -rf oznmon_horiz.fd
      $SLINK gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_horiz.fd         oznmon_horiz.fd

      [[ -d oznmon_time.fd ]] && rm -rf oznmon_time.fd
      $SLINK gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_time.fd          oznmon_time.fd

      [[ -d radmon_angle.fd ]] && rm -rf radmon_angle.fd
      $SLINK gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radang.fd       radmon_angle.fd

      [[ -d radmon_bcoef.fd ]] && rm -rf radmon_bcoef.fd
      $SLINK gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcoef.fd     radmon_bcoef.fd

      [[ -d radmon_bcor.fd ]] && rm -rf radmon_bcor.fd
      $SLINK gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcor.fd      radmon_bcor.fd

      [[ -d radmon_time.fd ]] && rm -rf radmon_time.fd
      $SLINK gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radtime.fd      radmon_time.fd
    fi

    [[ -d gfs_ncep_post.fd ]] && rm -rf gfs_ncep_post.fd
    $SLINK upp.fd/sorc/ncep_post.fd                                                   gfs_ncep_post.fd

    for prog in fregrid make_hgrid make_solo_mosaic ; do
        [[ -d ${prog}.fd ]] && rm -rf ${prog}.fd
        $SLINK ufs_utils.fd/sorc/fre-nctools.fd/tools/$prog                                ${prog}.fd
    done
    for prog in  global_cycle.fd \
        emcsfc_ice_blend.fd \
        emcsfc_snow2mdl.fd ;do
        [[ -d $prog ]] && rm -rf $prog
        $SLINK ufs_utils.fd/sorc/$prog                                                     $prog
    done


    if [ -d ${pwd}/gfs_wafs.fd ]; then
        $SLINK gfs_wafs.fd/sorc/wafs_awc_wafavn.fd                                              wafs_awc_wafavn.fd
        $SLINK gfs_wafs.fd/sorc/wafs_blending.fd                                                wafs_blending.fd
        $SLINK gfs_wafs.fd/sorc/wafs_blending_0p25.fd                                           wafs_blending_0p25.fd
        $SLINK gfs_wafs.fd/sorc/wafs_cnvgrib2.fd                                                wafs_cnvgrib2.fd
        $SLINK gfs_wafs.fd/sorc/wafs_gcip.fd                                                    wafs_gcip.fd
        $SLINK gfs_wafs.fd/sorc/wafs_grib2_0p25.fd                                              wafs_grib2_0p25.fd
        $SLINK gfs_wafs.fd/sorc/wafs_makewafs.fd                                                wafs_makewafs.fd
        $SLINK gfs_wafs.fd/sorc/wafs_setmissing.fd                                              wafs_setmissing.fd
    fi

    if [ -d gldas.fd ]; then
      for prog in gdas2gldas.fd  gldas2gdas.fd  gldas_forcing.fd  gldas_model.fd  gldas_post.fd  gldas_rst.fd ;do
        [[ -d $prog ]] && rm -rf $prog
        $SLINK gldas.fd/sorc/$prog                                                     $prog
      done
    fi

#------------------------------
#  copy $HOMEgfs/parm/config/config.base.nco.static as config.base for operations
#  config.base in the $HOMEgfs/parm/config has no use in development
cd $pwd/../parm/config
[[ -s config.base ]] && rm -f config.base
[[ $RUN_ENVIR = nco ]] && cp -p config.base.nco.static config.base
#------------------------------


exit 0

