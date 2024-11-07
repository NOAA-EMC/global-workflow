#!/bin/bash

#--make symbolic links for EMC installation and hardcopies for NCO delivery

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/.." >/dev/null 2>&1 && pwd )"
TRACE=NO source "${HOMEgfs}/ush/preamble.sh"

function usage() {
  cat << EOF
Builds all of the global-workflow components by calling the individual build
  scripts in sequence.

Usage: ${BASH_SOURCE[0]} [-h][-o][--nest]
  -h:
    Print this help message and exit
  -o:
    Configure for NCO (copy instead of link)
EOF
  exit 1
}

RUN_ENVIR="emc"

# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":ho-:" option; do
  case "${option}" in
    h) usage ;;
    o)
      echo "-o option received, configuring for NCO"
      RUN_ENVIR="nco";;
    -)
      if [[ "${OPTARG}" == "nest" ]]; then
        LINK_NEST=ON
      fi
      ;;
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

# LINK is always ln, LINK_OR_COPY can be ln or cp depending on RUN_ENVIR being emc or nco, respectively
LINK="ln -fs"
if [[ "${RUN_ENVIR}" == "nco" ]]; then
  LINK_OR_COPY="cp -rp"
else
  LINK_OR_COPY="ln -fs"
fi

# shellcheck disable=SC1091
COMPILER="intel" source "${HOMEgfs}/sorc/gfs_utils.fd/ush/detect_machine.sh"  # (sets MACHINE_ID)
# shellcheck disable=
machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

#------------------------------
#--Set up build.ver and run.ver
#------------------------------
${LINK_OR_COPY} "${HOMEgfs}/versions/build.${machine}.ver" "${HOMEgfs}/versions/build.ver"
${LINK_OR_COPY} "${HOMEgfs}/versions/run.${machine}.ver" "${HOMEgfs}/versions/run.ver"

#------------------------------
#--model fix fields
#------------------------------
case "${machine}" in
  "wcoss2")   FIX_DIR="/lfs/h2/emc/global/noscrub/emc.global/FIX/fix" ;;
  "hera")     FIX_DIR="/scratch1/NCEPDEV/global/glopara/fix" ;;
  "orion")    FIX_DIR="/work/noaa/global/glopara/fix" ;;
  "hercules") FIX_DIR="/work/noaa/global/glopara/fix" ;;
  "jet")      FIX_DIR="/lfs5/HFIP/hfv3gfs/glopara/FIX/fix" ;;
  "s4")       FIX_DIR="/data/prod/glopara/fix" ;;
  "gaea")     FIX_DIR="/gpfs/f5/ufs-ard/world-shared/global/glopara/data/fix" ;;
  "noaacloud") FIX_DIR="/contrib/global-workflow-shared-data/fix" ;;
  *)
    echo "FATAL: Unknown target machine ${machine}, couldn't set FIX_DIR"
    exit 1
    ;;
esac

# Source fix version file
source "${HOMEgfs}/versions/fix.ver"

# Link GDASapp python packages in ush/python
packages=("jcb")
for package in "${packages[@]}"; do
    cd "${HOMEgfs}/ush/python" || exit 1
    [[ -s "${package}" ]] && rm -f "${package}"
    ${LINK} "${HOMEgfs}/sorc/gdas.cd/sorc/${package}/src/${package}" .
done

# Link fix directories
if [[ -n "${FIX_DIR}" ]]; then
  if [[ ! -d "${HOMEgfs}/fix" ]]; then mkdir "${HOMEgfs}/fix" || exit 1; fi
fi
cd "${HOMEgfs}/fix" || exit 1
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
  ${LINK_OR_COPY} "${FIX_DIR}/${dir}/${!fix_ver}" "${dir}"
done
# global-nest uses different versions of orog and ugwd
if [[ "${LINK_NEST:-OFF}" == "ON" ]] ; then
  for dir in orog \
             ugwd
  do
    nestdir=${dir}_nest
    if [[ -d "${nestdir}" ]]; then
      [[ "${RUN_ENVIR}" == "nco" ]] && chmod -R 755 "${nestdir}"
      rm -rf "${nestdir}"
    fi
    fix_ver="${dir}_nest_ver"
    ${LINK_OR_COPY} "${FIX_DIR}/${dir}/${!fix_ver}" "${nestdir}"
  done
fi

#---------------------------------------
#--add files from external repositories
#---------------------------------------
#--copy/link NoahMp table form ccpp-physics repository
cd "${HOMEgfs}/parm/ufs" || exit 1
${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/parm/noahmptable.tbl" .

cd "${HOMEgfs}/parm/post" || exit 1
for file in params_grib2_tbl_new nam_micro_lookup.dat
do
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/parm/${file}" .
done
for dir in gfs gefs
do
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/parm/${dir}" .
done
for file in ice.csv ocean.csv ocnicepost.nml.jinja2
do
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gfs_utils.fd/parm/ocnicepost/${file}" .
done

cd "${HOMEgfs}/scripts" || exit 8
${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_utils.fd/scripts/exemcsfc_global_sfc_prep.sh" .
if [[ -d "${HOMEgfs}/sorc/gdas.cd" ]]; then
  declare -a gdas_scripts=(exglobal_prep_ocean_obs.py \
                           exgdas_global_marine_analysis_ecen.py \
                           )
  for gdas_script in "${gdas_scripts[@]}" ; do
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/scripts/${gdas_script}" .
  done
fi
cd "${HOMEgfs}/ush" || exit 8
for file in emcsfc_ice_blend.sh global_cycle_driver.sh emcsfc_snow.sh global_cycle.sh; do
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_utils.fd/ush/${file}" .
done
for file in make_ntc_bull.pl make_NTC_file.pl make_tif.sh month_name.sh ; do
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gfs_utils.fd/ush/${file}" .
done

# Link these templates from ufs-weather-model
cd "${HOMEgfs}/parm/ufs" || exit 1
declare -a ufs_templates=("model_configure.IN" "input_global_nest.nml.IN"\
                          "MOM_input_025.IN" "MOM_input_050.IN" "MOM_input_100.IN" "MOM_input_500.IN" \
                          "MOM6_data_table.IN" \
                          "ice_in.IN" \
                          "ufs.configure.atm.IN" \
                          "ufs.configure.atm_esmf.IN" \
                          "ufs.configure.atmaero.IN" \
                          "ufs.configure.atmaero_esmf.IN" \
                          "ufs.configure.s2s.IN" \
                          "ufs.configure.s2s_esmf.IN" \
                          "ufs.configure.s2sa.IN" \
                          "ufs.configure.s2sa_esmf.IN" \
                          "ufs.configure.s2sw.IN" \
                          "ufs.configure.s2sw_esmf.IN" \
                          "ufs.configure.s2swa.IN" \
                          "ufs.configure.s2swa_esmf.IN" \
                          "ufs.configure.leapfrog_atm_wav.IN" \
                          "ufs.configure.leapfrog_atm_wav_esmf.IN" \
                          "post_itag_gfs")
for file in "${ufs_templates[@]}"; do
  [[ -s "${file}" ]] && rm -f "${file}"
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/parm/${file}" .
done

# Link the script from ufs-weather-model that parses the templates
cd "${HOMEgfs}/ush" || exit 1
[[ -s "atparse.bash" ]] && rm -f "atparse.bash"
${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/atparse.bash" .


#------------------------------
#--add GDASApp fix directory
#------------------------------
if [[ -d "${HOMEgfs}/sorc/gdas.cd" ]]; then
  cd "${HOMEgfs}/fix" || exit 1
  [[ ! -d gdas ]] && mkdir -p gdas
  cd gdas || exit 1
  for gdas_sub in fv3jedi gsibec obs soca aero; do
    if [[ -d "${gdas_sub}" ]]; then
       rm -rf "${gdas_sub}"
    fi
    fix_ver="gdas_${gdas_sub}_ver"
    ${LINK_OR_COPY} "${FIX_DIR}/gdas/${gdas_sub}/${!fix_ver}" "${gdas_sub}"
  done
fi

#------------------------------
#--add GDASApp parm directory
#------------------------------
if [[ -d "${HOMEgfs}/sorc/gdas.cd" ]]; then
  cd "${HOMEgfs}/parm/gdas" || exit 1
  declare -a gdasapp_comps=("aero" "atm" "io" "ioda" "snow" "soca" "jcb-gdas" "jcb-algorithms")
  for comp in "${gdasapp_comps[@]}"; do
    [[ -d "${comp}" ]] && rm -rf "${comp}"
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/parm/${comp}" .
  done
fi

#------------------------------
#--add GDASApp files
#------------------------------
if [[ -d "${HOMEgfs}/sorc/gdas.cd/build" ]]; then
  cd "${HOMEgfs}/ush" || exit 1
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/soca"                              .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/ufsda"                              .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/jediinc2fv3.py"                     .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/ioda/bufr2ioda/gen_bufr2ioda_json.py"    .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/ioda/bufr2ioda/gen_bufr2ioda_yaml.py"    .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/ioda/bufr2ioda/run_bufr2ioda.py"    .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/build/bin/imsfv3_scf2ioda.py"           .
  declare -a gdasapp_ocn_insitu_profile_platforms=("argo" "bathy" "glider" "marinemammal" "tesac" "xbtctd")
  for platform in "${gdasapp_ocn_insitu_profile_platforms[@]}"; do
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/ioda/bufr2ioda/marine/bufr2ioda_insitu_profile_${platform}.py" .
  done
  declare -a gdasapp_ocn_insitu_sfc_platforms=("altkob" "trkob")
  for platform in "${gdasapp_ocn_insitu_sfc_platforms[@]}"; do
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/ush/ioda/bufr2ioda/marine/bufr2ioda_insitu_surface_${platform}.py" .
  done
fi

#------------------------------
#--add DA Monitor file (NOTE: ensure to use correct version)
#------------------------------
if [[ -d "${HOMEgfs}/sorc/gsi_monitor.fd" ]]; then

  cd "${HOMEgfs}/parm" || exit 1
  [[ -d monitor ]] && rm -rf monitor
  mkdir -p monitor
  cd monitor || exit 1
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_cost.txt" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_gnorm.txt" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/fix/gfs_minmon_cost.txt" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/fix/gfs_minmon_gnorm.txt" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_base.tar" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_satype.txt" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_base.tar" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_satype.txt" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_scaninfo.txt" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm" da_mon.parm
  # ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/parm/gdas_minmon.parm" .
  # ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/parm/gfs_minmon.parm" .
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/parm/gdas_oznmon.parm" .
  # ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm" .
fi

#------------------------------
#--link executables
#------------------------------

if [[ ! -d "${HOMEgfs}/exec" ]]; then mkdir "${HOMEgfs}/exec" || exit 1 ; fi
cd "${HOMEgfs}/exec" || exit 1

for utilexe in fbwndgfs.x gaussian_sfcanl.x gfs_bufr.x supvit.x syndat_getjtbul.x \
  syndat_maksynrc.x syndat_qctropcy.x tocsbufr.x overgridid.x rdbfmsua.x \
  mkgfsawps.x enkf_chgres_recenter_nc.x tave.x vint.x ocnicepost.x webtitle.x \
  ensadd.x ensppf.x ensstat.x wave_stat.x
do
  [[ -s "${utilexe}" ]] && rm -f "${utilexe}"
  ${LINK_OR_COPY} "${HOMEgfs}/sorc/gfs_utils.fd/install/bin/${utilexe}" .
done

[[ -s "ufs_model.x" ]] && rm -f ufs_model.x
${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/ufs_model.x" .

[[ -s "upp.x" ]] && rm -f upp.x
${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/exec/upp.x" .

for ufs_utilsexe in emcsfc_ice_blend emcsfc_snow2mdl global_cycle fregrid; do
    [[ -s "${ufs_utilsexe}" ]] && rm -f "${ufs_utilsexe}"
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_utils.fd/exec/${ufs_utilsexe}" .
done

# GSI
if [[ -d "${HOMEgfs}/sorc/gsi_enkf.fd/install" ]]; then
  for gsiexe in enkf.x gsi.x; do
    [[ -s "${gsiexe}" ]] && rm -f "${gsiexe}"
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_enkf.fd/install/bin/${gsiexe}" .
  done
fi

# GSI Utils
if [[ -d "${HOMEgfs}/sorc/gsi_utils.fd/install" ]]; then
  for exe in calc_analysis.x calc_increment_ens_ncio.x calc_increment_ens.x \
    getsfcensmeanp.x getsigensmeanp_smooth.x getsigensstatp.x \
    interp_inc.x recentersigp.x
  do
    [[ -s "${exe}" ]] && rm -f "${exe}"
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_utils.fd/install/bin/${exe}" .
  done
fi

# GSI Monitor
if [[ -d "${HOMEgfs}/sorc/gsi_monitor.fd/install" ]]; then
  for exe in oznmon_horiz.x oznmon_time.x radmon_angle.x \
    radmon_bcoef.x radmon_bcor.x radmon_time.x
  do
    [[ -s "${exe}" ]] && rm -f "${exe}"
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_monitor.fd/install/bin/${exe}" .
  done
fi

# GDASApp
if [[ -d "${HOMEgfs}/sorc/gdas.cd/build" ]]; then
  declare -a JEDI_EXE=("gdas.x" \
                       "gdas_soca_gridgen.x" \
                       "gdas_soca_error_covariance_toolbox.x" \
                       "gdas_fv3jedi_error_covariance_toolbox.x" \
                       "gdas_soca_setcorscales.x" \
                       "gdas_soca_diagb.x" \
                       "fv3jedi_plot_field.x" \
                       "gdasapp_chem_diagb.x" \
                       "fv3jedi_fv3inc.x" \
                       "gdas_ens_handler.x" \
                       "gdas_incr_handler.x" \
                       "gdas_obsprovider2ioda.x" \
                       "gdas_socahybridweights.x" \
                       "gdassoca_obsstats.x" \
                       "gdasapp_land_ensrecenter.x" \
                       "bufr2ioda.x" \
                       "calcfIMS.exe" \
                       "apply_incr.exe" )
  for gdasexe in "${JEDI_EXE[@]}"; do
    [[ -s "${gdasexe}" ]] && rm -f "${gdasexe}"
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/build/bin/${gdasexe}" .
  done
fi

#------------------------------
#--link source code directories
#------------------------------
cd "${HOMEgfs}/sorc" || exit 8
if [[ -d ufs_model.fd ]]; then
  [[ -d upp.fd ]] && rm -rf upp.fd
  ${LINK} ufs_model.fd/FV3/upp upp.fd
fi

if [[ -d gsi_enkf.fd ]]; then
  [[ -d gsi.fd ]] && rm -rf gsi.fd
  ${LINK} gsi_enkf.fd/src/gsi gsi.fd

  [[ -d enkf.fd ]] && rm -rf enkf.fd
  ${LINK} gsi_enkf.fd/src/enkf enkf.fd
fi

if [[ -d gsi_utils.fd ]]; then
  [[ -d calc_analysis.fd ]] && rm -rf calc_analysis.fd
  ${LINK} gsi_utils.fd/src/netcdf_io/calc_analysis.fd .

  [[ -d calc_increment_ens.fd ]] && rm -rf calc_increment_ens.fd
  ${LINK} gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens.fd .

  [[ -d calc_increment_ens_ncio.fd ]] && rm -rf calc_increment_ens_ncio.fd
  ${LINK} gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens_ncio.fd .

  [[ -d getsfcensmeanp.fd ]] && rm -rf getsfcensmeanp.fd
  ${LINK} gsi_utils.fd/src/EnKF/gfs/src/getsfcensmeanp.fd .

  [[ -d getsigensmeanp_smooth.fd ]] && rm -rf getsigensmeanp_smooth.fd
  ${LINK} gsi_utils.fd/src/EnKF/gfs/src/getsigensmeanp_smooth.fd .

  [[ -d getsigensstatp.fd ]] && rm -rf getsigensstatp.fd
  ${LINK} gsi_utils.fd/src/EnKF/gfs/src/getsigensstatp.fd .

  [[ -d recentersigp.fd ]] && rm -rf recentersigp.fd
  ${LINK} gsi_utils.fd/src/EnKF/gfs/src/recentersigp.fd .

  [[ -d interp_inc.fd ]] && rm -rf interp_inc.fd
  ${LINK} gsi_utils.fd/src/netcdf_io/interp_inc.fd .
fi

if [[ -d gsi_monitor.fd ]] ; then
  [[ -d oznmon_horiz.fd ]] && rm -rf oznmon_horiz.fd
  ${LINK} gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_horiz.fd .

  [[ -d oznmon_time.fd ]] && rm -rf oznmon_time.fd
  ${LINK} gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_time.fd .

  [[ -d radmon_angle.fd ]] && rm -rf radmon_angle.fd
  ${LINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radang.fd radmon_angle.fd

  [[ -d radmon_bcoef.fd ]] && rm -rf radmon_bcoef.fd
  ${LINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcoef.fd radmon_bcoef.fd

  [[ -d radmon_bcor.fd ]] && rm -rf radmon_bcor.fd
  ${LINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcor.fd radmon_bcor.fd

  [[ -d radmon_time.fd ]] && rm -rf radmon_time.fd
  ${LINK} gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radtime.fd radmon_time.fd
fi

for prog in global_cycle.fd emcsfc_ice_blend.fd emcsfc_snow2mdl.fd ;do
    [[ -d "${prog}" ]] && rm -rf "${prog}"
    ${LINK} "ufs_utils.fd/sorc/${prog}" "${prog}"
done

for prog in enkf_chgres_recenter_nc.fd \
  fbwndgfs.fd \
  gaussian_sfcanl.fd \
  gfs_bufr.fd \
  mkgfsawps.fd \
  overgridid.fd \
  rdbfmsua.fd \
  supvit.fd \
  syndat_getjtbul.fd \
  syndat_maksynrc.fd \
  syndat_qctropcy.fd \
  tave.fd \
  tocsbufr.fd \
  vint.fd \
  webtitle.fd \
  ocnicepost.fd
do
  if [[ -d "${prog}" ]]; then rm -rf "${prog}"; fi
  ${LINK_OR_COPY} "gfs_utils.fd/src/${prog}" .
done

exit 0
