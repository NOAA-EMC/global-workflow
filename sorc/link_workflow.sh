#!/bin/bash

#--make symbolic links for EMC installation and hardcopies for NCO delivery

trap 'echo "${BASH_SOURCE[0]} encounted an error at line ${LINENO} (rc=$?)"' ERR

function usage() {
  cat << EOF
Builds all of the global-workflow components by calling the individual build
  scripts in sequence.

Usage: ${BASH_SOURCE[0]} [-h][-o]
  -h:
    Print this help message and exit
  -o:
    Configure for NCO (copy instead of link)
EOF
  exit 1
}

set -eu

RUN_ENVIR="emc"

# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":ho" option; do
  case "${option}" in
    h) usage ;;
    o)
      echo "-o option received, configuring for NCO"
      RUN_ENVIR="nco";;
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      usage
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      usage
      ;;
  esac
done
shift $((OPTIND-1))

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
top_dir=$(cd "$(dirname "${script_dir}")" &> /dev/null && pwd)
cd "${script_dir}"

export COMPILER="intel"
# shellcheck disable=SC1091
source gfs_utils.fd/ush/detect_machine.sh  # (sets MACHINE_ID)
# shellcheck disable=
machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

#------------------------------
#--Set up build.ver and run.ver
#------------------------------
cp "${top_dir}/versions/build.${machine}.ver" "${top_dir}/versions/build.ver"
cp "${top_dir}/versions/run.${machine}.ver" "${top_dir}/versions/run.ver"

#------------------------------
#--model fix fields
#------------------------------
case "${machine}" in
  "wcoss2")   FIX_DIR="/lfs/h2/emc/global/noscrub/emc.global/FIX/fix" ;;
  "hera")     FIX_DIR="/scratch1/NCEPDEV/global/glopara/fix" ;;
  "orion")    FIX_DIR="/work/noaa/global/glopara/fix" ;;
  "jet")      FIX_DIR="/lfs4/HFIP/hfv3gfs/glopara/git/fv3gfs/fix" ;;
  "s4")       FIX_DIR="/data/prod/glopara/fix" ;;
  *)
    echo "FATAL: Unknown target machine ${machine}, couldn't set FIX_DIR"
    exit 1
    ;;
esac

# Source fix version file
source "${top_dir}/versions/fix.ver"

LINK="ln -fs"
SLINK="ln -fs"
if [[ "${RUN_ENVIR}" == "nco" ]]; then
  LINK="cp -rp"
fi

# Link post
[[ -d upp.fd ]] && rm -rf upp.fd
${LINK} ufs_model.fd/FV3/upp upp.fd

if [[ -n "${FIX_DIR}" ]]; then
  if [[ ! -d "${top_dir}/fix" ]]; then mkdir "${top_dir}/fix" || exit 1; fi
fi
cd "${top_dir}/fix" || exit 1
for dir in aer \
            am \
            chem \
            cice \
            cpl \
            datm \
            gsi \
            lut \
            mom6 \
            orog \
            reg2grb2 \
            sfc_climo \
            ugwd \
            verif \
            wave
            do
    if [[ -d "${dir}" ]]; then
      [[ "${RUN_ENVIR}" == "nco" ]] && chmod -R 755 "${dir}"
      rm -rf "${dir}"
    fi
    fix_ver="${dir}_ver"
    ${LINK} "${FIX_DIR}/${dir}/${!fix_ver}" "${dir}"
done


if [[ -d "${script_dir}/ufs_utils.fd" ]]; then
  cd "${script_dir}/ufs_utils.fd/fix" || exit 1
  ./link_fixdirs.sh "${RUN_ENVIR}" "${machine}" 2> /dev/null
fi


#---------------------------------------
#--add files from external repositories
#---------------------------------------
cd "${top_dir}/parm/post" || exit 1
    for file in postxconfig-NT-GEFS-ANL.txt postxconfig-NT-GEFS-F00.txt postxconfig-NT-GEFS.txt postxconfig-NT-GFS-ANL.txt \
        postxconfig-NT-GFS-F00-TWO.txt postxconfig-NT-GFS-F00.txt postxconfig-NT-GFS-FLUX-F00.txt postxconfig-NT-GFS-FLUX.txt \
        postxconfig-NT-GFS-GOES.txt postxconfig-NT-GFS-TWO.txt postxconfig-NT-GFS-WAFS-ANL.txt postxconfig-NT-GFS-WAFS.txt \
        postxconfig-NT-GFS.txt postxconfig-NT-gefs-aerosol.txt postxconfig-NT-gefs-chem.txt params_grib2_tbl_new \
        post_tag_gfs128 post_tag_gfs65 gtg.config.gfs gtg_imprintings.txt nam_micro_lookup.dat \
        AEROSOL_LUTS.dat optics_luts_DUST.dat optics_luts_SALT.dat optics_luts_SOOT.dat optics_luts_SUSO.dat optics_luts_WASO.dat \
        ; do
        ${LINK} "${script_dir}/upp.fd/parm/${file}" .
    done

cd "${top_dir}/scripts" || exit 8
    ${LINK} "${script_dir}/ufs_utils.fd/scripts/exemcsfc_global_sfc_prep.sh" .
cd "${top_dir}/ush" || exit 8
    for file in emcsfc_ice_blend.sh  fv3gfs_driver_grid.sh  fv3gfs_make_orog.sh  global_cycle_driver.sh \
        emcsfc_snow.sh  fv3gfs_filter_topo.sh  global_cycle.sh  fv3gfs_make_grid.sh ; do
        ${LINK} "${script_dir}/ufs_utils.fd/ush/${file}" .
    done
    for file in finddate.sh  make_ntc_bull.pl  make_NTC_file.pl  make_tif.sh  month_name.sh ; do
        ${LINK} "${script_dir}/gfs_utils.fd/ush/${file}" .
    done

#-----------------------------------
#--add gfs_wafs link if checked out
if [[ -d "${script_dir}/gfs_wafs.fd" ]]; then
#-----------------------------------
 cd "${top_dir}/jobs" || exit 1
     ${LINK} "${script_dir}/gfs_wafs.fd/jobs"/*                         .
 cd "${top_dir}/parm" || exit 1
     [[ -d wafs ]] && rm -rf wafs
    ${LINK} "${script_dir}/gfs_wafs.fd/parm/wafs"                      wafs
 cd "${top_dir}/scripts" || exit 1
    ${LINK} "${script_dir}/gfs_wafs.fd/scripts"/*                      .
 cd "${top_dir}/ush" || exit 1
    ${LINK} "${script_dir}/gfs_wafs.fd/ush"/*                          .
 cd "${top_dir}/fix" || exit 1
    [[ -d wafs ]] && rm -rf wafs
    ${LINK} "${script_dir}/gfs_wafs.fd/fix"/*                          .
fi


#------------------------------
#--add GDASApp fix directory
#------------------------------
if [[ -d "${script_dir}/gdas.cd" ]]; then
  cd "${top_dir}/fix" || exit 1
    [[ ! -d gdas ]] && mkdir -p gdas
    cd gdas || exit 1
    for gdas_sub in crtm fv3jedi gsibec; do
      if [[ -d "${gdas_sub}" ]]; then
         rm -rf "${gdas_sub}"
      fi
      fix_ver="gdas_${gdas_sub}_ver"
      ${LINK} "${FIX_DIR}/gdas/${gdas_sub}/${!fix_ver}" "${gdas_sub}"
    done
fi

#------------------------------
#--add GDASApp files
#------------------------------
if [[ -d "${script_dir}/gdas.cd" ]]; then
  cd "${top_dir}/ush" || exit 1
    ${LINK} "${script_dir}/gdas.cd/ush/ufsda"                              .
    ${LINK} "${script_dir}/gdas.cd/ush/jediinc2fv3.py"                     .
    ${LINK} "${script_dir}/gdas.cd/build/bin/imsfv3_scf2ioda.py"           .
    ${LINK} "${script_dir}/gdas.cd/ush/land/letkf_create_ens.py"           .
fi


#------------------------------
#--add DA Monitor file (NOTE: ensure to use correct version)
#------------------------------
if [[ -d "${script_dir}/gsi_monitor.fd" ]]; then

  cd "${top_dir}/fix" || exit 1
    [[ ! -d gdas ]] && ( mkdir -p gdas || exit 1 )
    cd gdas || exit 1
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_cost.txt"                   .
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_gnorm.txt"                  .
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_base.tar"                   .
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_satype.txt"                 .
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_base.tar"                .
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_satype.txt"              .
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_scaninfo.txt"            .
  cd "${top_dir}/parm" || exit 1
    [[ -d mon ]] && rm -rf mon
    mkdir -p mon
    cd mon || exit 1
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm"                   da_mon.parm
    # ${LINK} "${script_dir}/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/parm/gdas_minmon.parm"                      .
    # ${LINK} "${script_dir}/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/parm/gfs_minmon.parm"                        .
    ${LINK} "${script_dir}/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/parm/gdas_oznmon.parm"                      .
    # ${LINK} "${script_dir}/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm"                   .
fi

#------------------------------
#--link executables
#------------------------------

if [[ ! -d "${top_dir}/exec" ]]; then mkdir "${top_dir}/exec" || exit 1 ; fi
cd "${top_dir}/exec" || exit 1

for utilexe in fbwndgfs.x gaussian_sfcanl.x gfs_bufr.x regrid_nemsio.x supvit.x syndat_getjtbul.x \
  syndat_maksynrc.x syndat_qctropcy.x tocsbufr.x enkf_chgres_recenter.x overgridid.x \
  mkgfsawps.x enkf_chgres_recenter_nc.x fv3nc2nemsio.x tave.x vint.x reg2grb2.x ; do
    [[ -s "${utilexe}" ]] && rm -f "${utilexe}"
    ${LINK} "${script_dir}/gfs_utils.fd/install/bin/${utilexe}" .
done

[[ -s "ufs_model.x" ]] && rm -f ufs_model.x
${LINK} "${script_dir}/ufs_model.fd/tests/ufs_model.x" .

[[ -s "upp.x" ]] && rm -f upp.x
${LINK} "${script_dir}/upp.fd/exec/upp.x" .

if [[ -d "${script_dir}/gfs_wafs.fd" ]]; then
    for wafsexe in \
          wafs_awc_wafavn.x  wafs_blending.x  wafs_blending_0p25.x \
          wafs_cnvgrib2.x  wafs_gcip.x  wafs_grib2_0p25.x \
          wafs_makewafs.x  wafs_setmissing.x; do
        [[ -s ${wafsexe} ]] && rm -f "${wafsexe}"
        ${LINK} "${script_dir}/gfs_wafs.fd/exec/${wafsexe}" .
    done
fi

for ufs_utilsexe in \
     emcsfc_ice_blend  emcsfc_snow2mdl  global_cycle ; do
    [[ -s "${ufs_utilsexe}" ]] && rm -f "${ufs_utilsexe}"
    ${LINK} "${script_dir}/ufs_utils.fd/exec/${ufs_utilsexe}" .
done

# GSI
if [[ -d "${script_dir}/gsi_enkf.fd" ]]; then
  for gsiexe in enkf.x gsi.x; do
    [[ -s "${gsiexe}" ]] && rm -f "${gsiexe}"
    ${LINK} "${script_dir}/gsi_enkf.fd/install/bin/${gsiexe}" .
  done
fi

# GSI Utils
if [[ -d "${script_dir}/gsi_utils.fd" ]]; then
  for exe in calc_analysis.x calc_increment_ens_ncio.x calc_increment_ens.x \
    getsfcensmeanp.x getsigensmeanp_smooth.x getsigensstatp.x \
    interp_inc.x recentersigp.x;do
    [[ -s "${exe}" ]] && rm -f "${exe}"
    ${LINK} "${script_dir}/gsi_utils.fd/install/bin/${exe}" .
  done
fi

# GSI Monitor
if [[ -d "${script_dir}/gsi_monitor.fd" ]]; then
  for exe in oznmon_horiz.x oznmon_time.x radmon_angle.x \
    radmon_bcoef.x radmon_bcor.x radmon_time.x; do
    [[ -s "${exe}" ]] && rm -f "${exe}"
    ${LINK} "${script_dir}/gsi_monitor.fd/install/bin/${exe}" .
  done
fi

# GDASApp
if [[ -d "${script_dir}/gdas.cd" ]]; then
  declare -a JEDI_EXE=("fv3jedi_addincrement.x" \
                       "fv3jedi_diffstates.x" \
                       "fv3jedi_ensvariance.x" \
                       "fv3jedi_hofx.x" \
                       "fv3jedi_var.x" \
                       "fv3jedi_convertincrement.x" \
                       "fv3jedi_dirac.x" \
                       "fv3jedi_error_covariance_training.x" \
                       "fv3jedi_letkf.x" \
                       "fv3jedi_convertstate.x" \
                       "fv3jedi_eda.x" \
                       "fv3jedi_forecast.x" \
                       "fv3jedi_plot_field.x" \
                       "fv3jedi_data_checker.py" \
                       "fv3jedi_enshofx.x" \
                       "fv3jedi_hofx_nomodel.x" \
                       "fv3jedi_testdata_downloader.py" \
                       "soca_convertincrement.x" \
                       "soca_error_covariance_training.x" \
                       "soca_setcorscales.x" \
                       "soca_gridgen.x" \
                       "soca_var.x" \
                       "calcfIMS.exe" \
                       "apply_incr.exe" )
  for gdasexe in "${JEDI_EXE[@]}"; do
    [[ -s "${gdasexe}" ]] && rm -f "${gdasexe}"
    ${LINK} "${script_dir}/gdas.cd/build/bin/${gdasexe}" .
  done
fi

#------------------------------
#--link source code directories
#------------------------------
cd "${script_dir}" || exit 8

    if [[ -d gsi_enkf.fd ]]; then
      [[ -d gsi.fd ]] && rm -rf gsi.fd
      ${SLINK} gsi_enkf.fd/src/gsi                                                                gsi.fd

      [[ -d enkf.fd ]] && rm -rf enkf.fd
      ${SLINK} gsi_enkf.fd/src/enkf                                                               enkf.fd
    fi

    if [[ -d gsi_utils.fd ]]; then
      [[ -d calc_analysis.fd ]] && rm -rf calc_analysis.fd
      ${SLINK} gsi_utils.fd/src/netcdf_io/calc_analysis.fd                                        calc_analysis.fd

      [[ -d calc_increment_ens.fd ]] && rm -rf calc_increment_ens.fd
      ${SLINK} gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens.fd                                calc_increment_ens.fd

      [[ -d calc_increment_ens_ncio.fd ]] && rm -rf calc_increment_ens_ncio.fd
      ${SLINK} gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens_ncio.fd                           calc_increment_ens_ncio.fd

      [[ -d getsfcensmeanp.fd ]] && rm -rf getsfcensmeanp.fd
      ${SLINK} gsi_utils.fd/src/EnKF/gfs/src/getsfcensmeanp.fd                                    getsfcensmeanp.fd

      [[ -d getsigensmeanp_smooth.fd ]] && rm -rf getsigensmeanp_smooth.fd
      ${SLINK} gsi_utils.fd/src/EnKF/gfs/src/getsigensmeanp_smooth.fd                             getsigensmeanp_smooth.fd

      [[ -d getsigensstatp.fd ]] && rm -rf getsigensstatp.fd
      ${SLINK} gsi_utils.fd/src/EnKF/gfs/src/getsigensstatp.fd                                    getsigensstatp.fd

      [[ -d recentersigp.fd ]] && rm -rf recentersigp.fd
      ${SLINK} gsi_utils.fd/src/EnKF/gfs/src/recentersigp.fd                                      recentersigp.fd

      [[ -d interp_inc.fd ]] && rm -rf interp_inc.fd
      ${SLINK} gsi_utils.fd/src/netcdf_io/interp_inc.fd                                           interp_inc.fd
    fi

    if [[ -d gsi_monitor.fd ]] ; then
      [[ -d oznmon_horiz.fd ]] && rm -rf oznmon_horiz.fd
      ${SLINK} gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_horiz.fd         oznmon_horiz.fd

      [[ -d oznmon_time.fd ]] && rm -rf oznmon_time.fd
      ${SLINK} gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_time.fd          oznmon_time.fd

      [[ -d radmon_angle.fd ]] && rm -rf radmon_angle.fd
      ${SLINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radang.fd       radmon_angle.fd

      [[ -d radmon_bcoef.fd ]] && rm -rf radmon_bcoef.fd
      ${SLINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcoef.fd     radmon_bcoef.fd

      [[ -d radmon_bcor.fd ]] && rm -rf radmon_bcor.fd
      ${SLINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcor.fd      radmon_bcor.fd

      [[ -d radmon_time.fd ]] && rm -rf radmon_time.fd
      ${SLINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radtime.fd      radmon_time.fd
    fi

    [[ -d gfs_ncep_post.fd ]] && rm -rf gfs_ncep_post.fd
    ${SLINK} upp.fd/sorc/ncep_post.fd                                                   gfs_ncep_post.fd

    for prog in fregrid make_hgrid make_solo_mosaic ; do
        [[ -d "${prog}.fd" ]] && rm -rf "${prog}.fd"
        ${SLINK} "ufs_utils.fd/sorc/fre-nctools.fd/tools/${prog}"                                "${prog}.fd"
    done
    for prog in global_cycle.fd \
        emcsfc_ice_blend.fd \
        emcsfc_snow2mdl.fd ;do
        [[ -d "${prog}" ]] && rm -rf "${prog}"
        ${SLINK} "ufs_utils.fd/sorc/${prog}"                                                     "${prog}"
    done

    for prog in enkf_chgres_recenter.fd \
      enkf_chgres_recenter_nc.fd \
      fbwndgfs.fd \
      fv3nc2nemsio.fd \
      gaussian_sfcanl.fd \
      gfs_bufr.fd \
      mkgfsawps.fd \
      overgridid.fd \
      rdbfmsua.fd \
      reg2grb2.fd \
      regrid_nemsio.fd \
      supvit.fd \
      syndat_getjtbul.fd \
      syndat_maksynrc.fd \
      syndat_qctropcy.fd \
      tave.fd \
      tocsbufr.fd \
      vint.fd \
      webtitle.fd
      do
        if [[ -d "${prog}" ]]; then rm -rf "${prog}"; fi
        ${LINK} "gfs_utils.fd/src/${prog}" .
    done

    if [[ -d "${script_dir}/gfs_wafs.fd" ]]; then
        ${SLINK} gfs_wafs.fd/sorc/wafs_awc_wafavn.fd                                              wafs_awc_wafavn.fd
        ${SLINK} gfs_wafs.fd/sorc/wafs_blending.fd                                                wafs_blending.fd
        ${SLINK} gfs_wafs.fd/sorc/wafs_blending_0p25.fd                                           wafs_blending_0p25.fd
        ${SLINK} gfs_wafs.fd/sorc/wafs_cnvgrib2.fd                                                wafs_cnvgrib2.fd
        ${SLINK} gfs_wafs.fd/sorc/wafs_gcip.fd                                                    wafs_gcip.fd
        ${SLINK} gfs_wafs.fd/sorc/wafs_grib2_0p25.fd                                              wafs_grib2_0p25.fd
        ${SLINK} gfs_wafs.fd/sorc/wafs_makewafs.fd                                                wafs_makewafs.fd
        ${SLINK} gfs_wafs.fd/sorc/wafs_setmissing.fd                                              wafs_setmissing.fd
    fi

#------------------------------
#  copy $HOMEgfs/parm/config/config.base.nco.static as config.base for operations
#  config.base in the $HOMEgfs/parm/config has no use in development
cd "${top_dir}/parm/config" || exit 1
[[ -s "config.base" ]] && rm -f config.base
if [[ "${RUN_ENVIR}" == "nco" ]] ; then
  cp -p config.base.nco.static config.base
  cp -p config.fv3.nco.static config.fv3
  cp -p config.resources.nco.static config.resources
fi
#------------------------------

echo "${BASH_SOURCE[0]} completed successfully"

exit 0
