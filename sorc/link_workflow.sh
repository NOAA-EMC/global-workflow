#!/bin/bash

#--make symbolic links for EMC installation and hardcopies for NCO delivery

HOMEglobal=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")" > /dev/null 2>&1 && git rev-parse --show-toplevel)

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
            RUN_ENVIR="nco"
            ;;
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
shift $((OPTIND - 1))

# LINK is always ln, LINK_OR_COPY can be ln or cp depending on RUN_ENVIR being emc or nco, respectively
LINK="ln -fs"
if [[ "${RUN_ENVIR}" == "nco" ]]; then
    LINK_OR_COPY="cp -rp"
else
    LINK_OR_COPY="ln -fs"
fi

# Re-linking a directory under a name isn't idempotent: if link_workflow.sh is
# called again, the previous run's link at dest makes ln/cp nest the result
# inside it instead of replacing it. A similar issue occurs with spurious copying.
# guard() deletes the resolved target first so reruns stay clean.
function guard() {
    local src=$1
    local dest=$2

    # "." (or "dir/", "dir/.") means ln/cp place the result *inside* that
    # directory as dest/<basename>; anything else is the literal target name.
    # Resolve that path so we remove exactly what the link/copy will create.
    local link_name
    if [[ "${dest}" == "." ]]; then
        link_name="$(basename "${src}")"
    elif [[ "${dest}" == */ || "${dest}" == */. ]]; then
        link_name="${dest%/*}/$(basename "${src}")"
    else
        link_name="${dest}"
    fi

    # guard requires permission to delete existing copies of ${link_name}
    if [[ "${RUN_ENVIR}" == "nco" && -d "${link_name}" && ! -L "${link_name}" ]]; then
        chmod -R 755 "${link_name}"
    fi

    # clean up the resolved name to prevent recursive linking / nested copies
    rm -rf "${link_name}"
}

# this wrapper for ${LINK} calls the guard to link safely
# usage: safe_link <src> <dest>
function safe_link() {
    guard "$1" "$2"
    ${LINK} "$1" "$2"
}

# this wrapper for ${LINK_OR_COPY} calls the guard to link/copy safely
# usage: safe_link <src> <dest>
function safe_link_or_copy() {
    guard "$1" "$2"
    ${LINK_OR_COPY} "$1" "$2"
}

# shellcheck disable=SC1091
COMPILER="intel" source "${HOMEglobal}/ush/detect_machine.sh" # (sets MACHINE_ID)
# shellcheck disable=
machine=$(echo "${MACHINE_ID}" | cut -d. -f1)

#------------------------------
#--Set up build.ver and run.ver
#------------------------------
safe_link_or_copy "${HOMEglobal}/versions/build.${machine}.ver" "${HOMEglobal}/versions/build.ver"
safe_link_or_copy "${HOMEglobal}/versions/run.${machine}.ver" "${HOMEglobal}/versions/run.ver"

#------------------------------
#--model fix fields
#------------------------------
case "${machine}" in
    "wcoss2") FIX_DIR="/lfs/h2/emc/global/noscrub/emc.global/FIX/fix" ;;
    "hera" | "ursa") FIX_DIR="/scratch3/NCEPDEV/global/role.glopara/fix" ;;
    "orion") FIX_DIR="/work2/noaa/global/role-global/fix" ;;
    "hercules") FIX_DIR="/work2/noaa/global/role-global/fix" ;;
    "gaeac6") FIX_DIR="/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix" ;;
    "aws-ec2") FIX_DIR="/lustre/global/data/fix" ;;
    "derecho") FIX_DIR="/lustre/desc1/p/nral0032/global/data/fix" ;;
    "noaacloud") FIX_DIR="/lustre/fix" ;;
    *)
        echo "FATAL: Unknown target machine ${machine}, couldn't set FIX_DIR"
        exit 1
        ;;
esac

# Source fix version file
source "${HOMEglobal}/versions/fix.ver"

# Link GDASapp python packages in ush/python
packages=("jcb")
for package in "${packages[@]}"; do
    cd "${HOMEglobal}/ush/python" || exit 1
    safe_link "${HOMEglobal}/sorc/gdas.cd/sorc/${package}/src/${package}" .
done

# Link wxflow to ush/python
cd "${HOMEglobal}/ush/python" || exit 1
if [[ -d "${HOMEglobal}/sorc/wxflow/src/wxflow" ]]; then
    safe_link "${HOMEglobal}/sorc/wxflow/src/wxflow" .
fi

# Link fix directories
if [[ -n "${FIX_DIR}" ]]; then
    mkdir -p "${HOMEglobal}/fix" || exit 1
fi
cd "${HOMEglobal}/fix" || exit 1
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
    wave; do
    fix_ver="${dir}_ver"
    safe_link_or_copy "${FIX_DIR}/${dir}/${!fix_ver}" "${dir}"
done
# global-nest uses different versions of orog and ugwd
if [[ "${LINK_NEST:-OFF}" == "ON" ]]; then
    for dir in orog \
        ugwd; do
        nestdir=${dir}_nest
        fix_ver="${dir}_nest_ver"
        safe_link_or_copy "${FIX_DIR}/${dir}/${!fix_ver}" "${nestdir}"
    done
fi

#---------------------------------------
#--link sorc/upp.fd before referencing files within it
#---------------------------------------
cd "${HOMEglobal}/sorc" || exit 8
if [[ -d ufs_model.fd ]]; then
    safe_link ufs_model.fd/UFSATM/upp upp.fd
fi
#---------------------------------------
#--add files from external repositories
#---------------------------------------
#--copy/link NoahMp table form ccpp-physics repository
cd "${HOMEglobal}/parm/ufs" || exit 1
safe_link_or_copy "${HOMEglobal}/sorc/ufs_model.fd/tests/parm/noahmptable.tbl" .
safe_link_or_copy "${HOMEglobal}/sorc/ufs_model.fd/tests/parm/fd_ufs.yaml" .

cd "${HOMEglobal}/parm/post" || exit 1
safe_link_or_copy "${HOMEglobal}/sorc/upp.fd/parm/params_grib2_tbl_new" .
safe_link_or_copy "${HOMEglobal}/sorc/upp.fd/fix/nam_micro_lookup.dat" .

for dir in gfs gcafs gefs sfs; do
    safe_link_or_copy "${HOMEglobal}/sorc/upp.fd/parm/${dir}" .
done

for file in optics_luts_DUST.dat optics_luts_DUST_nasa.dat optics_luts_NITR_nasa.dat \
    optics_luts_SALT.dat optics_luts_SALT_nasa.dat optics_luts_SOOT.dat optics_luts_SOOT_nasa.dat \
    optics_luts_SUSO.dat optics_luts_SUSO_nasa.dat optics_luts_WASO.dat optics_luts_WASO_nasa.dat; do
    safe_link_or_copy "${HOMEglobal}/sorc/upp.fd/fix/chem/${file}" .
done

for file in ice_gfs.csv ice_gefs.csv ocean_gfs.csv ocean_gefs.csv ocnicepost.nml.jinja2; do
    safe_link_or_copy "${HOMEglobal}/sorc/gfs_utils.fd/parm/ocnicepost/${file}" .
done

cd "${HOMEglobal}/scripts" || exit 8
if [[ -d "${HOMEglobal}/sorc/gdas.cd" ]]; then
    declare -a gdas_scripts=(exglobal_prep_ocean_obs.py)
    for gdas_script in "${gdas_scripts[@]}"; do
        safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/scripts/${gdas_script}" .
    done
fi

# Link these templates from ufs-weather-model
cd "${HOMEglobal}/parm/ufs" || exit 1
declare -a ufs_templates=("model_configure.IN" "input_global_nest.nml.IN"
    "MOM_input_025.IN" "MOM_input_050.IN" "MOM_input_100.IN" "MOM_input_500.IN"
    "MOM6_data_table.IN"
    "ice_in.IN"
    "ufs.configure.atm.IN"
    "ufs.configure.atmaero.IN"
    "ufs.configure.s2s.IN"
    "ufs.configure.s2sa.IN"
    "ufs.configure.s2sw.IN"
    "ufs.configure.s2swa.IN"
    "ufs.configure.leapfrog_atm_wav.IN"
    "ww3_shel.nml.IN"
    "post_itag_gfs"
    "post_itag_gcafs"
    "global_control.nml.IN")

for file in "${ufs_templates[@]}"; do
    safe_link_or_copy "${HOMEglobal}/sorc/ufs_model.fd/tests/parm/${file}" .
done

# Link the CCPP suite XML files from ufs-weather-model
declare -a ccpp_suites=(
    "suite_FV3_global_nest_v1.xml"
    "suite_FV3_GFS_v17_p8_ugwpv1.xml"
    "suite_FV3_GFS_v17_coupled_p8_ugwpv1.xml"
)
if [[ -d "${HOMEglobal}/sorc/ufs_model.fd/UFSATM/ccpp/suites" ]]; then
    for suite_file in "${ccpp_suites[@]}"; do
        src="${HOMEglobal}/sorc/ufs_model.fd/UFSATM/ccpp/suites/${suite_file}"
        [[ -f "${src}" ]] || continue
        safe_link_or_copy "${src}" .
    done
fi

# Link the script from ufs-weather-model that parses the templates
cd "${HOMEglobal}/ush" || exit 1
safe_link_or_copy "${HOMEglobal}/sorc/ufs_model.fd/tests/atparse.bash" .

# Link UPP modulefiles for module loading
cd "${HOMEglobal}/modulefiles" || exit 1
if [[ -d "${HOMEglobal}/sorc/ufs_model.fd/UFSATM/upp/modulefiles" ]]; then
    safe_link_or_copy "${HOMEglobal}/sorc/ufs_model.fd/UFSATM/upp/modulefiles" upp
fi

# add ufs_utils parm dir
if [[ -d "${HOMEglobal}/sorc/ufs_utils.fd" ]]; then
    cd "${HOMEglobal}/parm" || exit 1
    mkdir -p regrid_sfc
    cd regrid_sfc || exit 1
    safe_link_or_copy "${HOMEglobal}/sorc/ufs_utils.fd/parm/regrid_sfc/regrid.nml_tmpl" .
fi

#------------------------------
#--add GDASApp fix directory
#------------------------------
if [[ -d "${HOMEglobal}/sorc/gdas.cd" ]]; then
    cd "${HOMEglobal}/fix" || exit 1
    mkdir -p gdas
    cd gdas || exit 1
    for gdas_sub in fv3jedi gsibec obs soca aero snow soil; do
        fix_ver="gdas_${gdas_sub}_ver"
        safe_link_or_copy "${FIX_DIR}/gdas/${gdas_sub}/${!fix_ver}" "${gdas_sub}"
    done
fi

#------------------------------
#--add GDASApp parm directory
#------------------------------
if [[ -d "${HOMEglobal}/sorc/gdas.cd" ]]; then
    cd "${HOMEglobal}/parm" || exit 1
    mkdir -p gdas
    cd gdas || exit 1
    declare -a gdasapp_comps=("aero" "atm" "io" "ioda" "snow" "soil" "marine" "jcb-gdas" "jcb-algorithms" "anlstat" "analcalc")
    for comp in "${gdasapp_comps[@]}"; do
        safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/parm/${comp}" .
    done
fi

#------------------------------
#--add SPOC parm and ush directory
#------------------------------
sources=("config" "scripts")
targets=("parm/gdas" "ush")
for i in "${!sources[@]}"; do
    src="${HOMEglobal}/sorc/gdas.cd/sorc/spoc/dump/${sources[${i}]}"
    dst="${HOMEglobal}/${targets[${i}]}"

    if [[ -d "${src}" ]]; then
        cd "${dst}" || exit 1
        safe_link_or_copy "${src}" "spoc"
    fi
done

#------------------------------
#--add GDASApp files
#------------------------------
if [[ -d "${HOMEglobal}/sorc/gdas.cd/build" ]]; then
    cd "${HOMEglobal}/ush/python" || exit 1
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/soca" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/ufsda" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/ioda/bufr2ioda/gen_bufr2ioda_json.py" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/ioda/bufr2ioda/gen_bufr2ioda_yaml.py" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/ioda/bufr2ioda/run_bufr2ioda.py" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/sorc/da-utils/ush/gsincdiag_to_ioda" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/sorc/da-utils/ush/pyiodaconv" .
    cd "${HOMEglobal}/ush" || exit 1
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/gsi_satbias2ioda_all.sh" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/snow/bufr_snocvr_snomad.py" .
    safe_link_or_copy "${HOMEglobal}/sorc/gdas.cd/ush/snow/ghcn_snod2ioda.py" .
fi

#------------------------------
#--add DA Monitor file (NOTE: ensure to use correct version)
#------------------------------
if [[ -d "${HOMEglobal}/sorc/gsi_monitor.fd" ]]; then

    cd "${HOMEglobal}/parm" || exit 1
    if [[ -d monitor ]]; then
        rm -rf monitor
    fi
    mkdir -p monitor
    cd monitor || exit 1
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_cost.txt" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/fix/gdas_minmon_gnorm.txt" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/fix/gfs_minmon_cost.txt" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/fix/gfs_minmon_gnorm.txt" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_base.tar" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/fix/gdas_oznmon_satype.txt" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_base.tar" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_satype.txt" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/fix/gdas_radmon_scaninfo.txt" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm" da_mon.parm
    # safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gdas/parm/gdas_minmon.parm" .
    # safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Minimization_Monitor/nwprod/gfs/parm/gfs_minmon.parm" .
    safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Ozone_Monitor/nwprod/gdas_oznmon/parm/gdas_oznmon.parm" .
    # safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/src/Radiance_Monitor/nwprod/gdas_radmon/parm/gdas_radmon.parm" .
fi

#-------------------------------------------
#--Add GSI conv, sat, and oz info parm files
#-------------------------------------------
if [[ -d "${HOMEglobal}/sorc/gsi_enkf.fd/fix/build_gsinfo" ]]; then

    cd "${HOMEglobal}/parm" || exit 1

    mkdir -p gsinfo

    cd gsinfo || exit 1

    for dir in convinfo satinfo ozinfo obs_input hirs_fix; do
        safe_link_or_copy "${HOMEglobal}/sorc/gsi_enkf.fd/fix/build_gsinfo/${dir}" "${dir}"
    done
fi

#------------------------------
#--add NEXUS files
#------------------------------
if [[ -d "${HOMEglobal}/sorc/nexus.fd" ]]; then
    cd "${HOMEglobal}/parm/chem" || exit 1
    if [[ -d nexus ]]; then
        rm -rf nexus
    fi
    mkdir -p nexus/gocart
    cd nexus/gocart || exit 1
    safe_link_or_copy "${HOMEglobal}/sorc/nexus.fd/config/gocart/NEXUS_Config.rc.j2" .
    safe_link_or_copy "${HOMEglobal}/sorc/nexus.fd/config/gocart/HEMCO_sa_Grid.rc.j2" .
    safe_link_or_copy "${HOMEglobal}/sorc/nexus.fd/config/gocart/HEMCO_sa_Time.rc.j2" .
    safe_link_or_copy "${HOMEglobal}/sorc/nexus.fd/config/gocart/HEMCO_sa_Diag.rc.j2" .
    safe_link_or_copy "${HOMEglobal}/sorc/nexus.fd/config/gocart/HEMCO_sa_Spec.rc.j2" .
fi

#------------------------------
#--link executables
#------------------------------

mkdir -p "${HOMEglobal}/exec" || exit 1

cd "${HOMEglobal}/exec" || exit 1

for utilexe in fbwndgfs.x gaussian_sfcanl.x gfs_bufr.x supvit.x syndat_getjtbul.x \
    syndat_maksynrc.x syndat_qctropcy.x tocsbufr.x overgridid.x rdbfmsua.x \
    mkgfsawps.x enkf_chgres_recenter_nc.x tave.x vint.x ocnicepost.x webtitle.x \
    ensadd.x ensppf.x ensstat.x wave_stat.x tref_calc.x; do
    safe_link_or_copy "${HOMEglobal}/sorc/gfs_utils.fd/install/bin/${utilexe}" .
done

declare -a model_systems=("gfs" "gefs" "sfs" "gcafs")
for sys in "${model_systems[@]}"; do
    model_exe="${sys}_model.x"
    # unconditionally remove the destination of a conditional safe_link_or_copy
    if [[ -s "ufs_model_${sys}.x" ]]; then
        rm -f "ufs_model_${sys}.x"
    fi
    if [[ -f "${HOMEglobal}/sorc/ufs_model.fd/tests/${model_exe}" ]]; then
        safe_link_or_copy "${HOMEglobal}/sorc/ufs_model.fd/tests/${model_exe}" "ufs_model_${sys}.x"
    fi
done

# WW3 pre/post executables
declare -a ww3_exes=("ww3_grid" "ww3_prnc" "ww3_outp" "ww3_gint" "ww3_grib")
declare -A wave_systems
wave_systems["gfs"]="pdlib_ON"
wave_systems["gefs"]="pdlib_OFF"
wave_systems["sfs"]="pdlib_OFF"

for sys in "${!wave_systems[@]}"; do
    build_loc="${wave_systems[${sys}]}"
    if [[ -d "${HOMEglobal}/sorc/ufs_model.fd/WW3/install/${build_loc}" ]]; then
        for ww3exe in "${ww3_exes[@]}"; do
            target_ww3_exe="${ww3exe}_${sys}.x"
            safe_link_or_copy "${HOMEglobal}/sorc/ufs_model.fd/WW3/install/${build_loc}/bin/${ww3exe}" "${HOMEglobal}/exec/${target_ww3_exe}"
        done
    fi
done

safe_link_or_copy "${HOMEglobal}/sorc/upp.fd/exec/upp.x" .

for ufs_utilsexe in chgres_cube emcsfc_ice_blend emcsfc_snow2mdl global_cycle regridStates.x; do
    safe_link_or_copy "${HOMEglobal}/sorc/ufs_utils.fd/exec/${ufs_utilsexe}" .
done

# GSI
if [[ -d "${HOMEglobal}/sorc/gsi_enkf.fd/install" ]]; then
    for gsiexe in enkf.x gsi.x; do
        safe_link_or_copy "${HOMEglobal}/sorc/gsi_enkf.fd/install/bin/${gsiexe}" .
    done
fi

# GSI Utils
if [[ -d "${HOMEglobal}/sorc/gsi_utils.fd/install" ]]; then
    for exe in calc_analysis.x calc_increment_ens_ncio.x calc_increment_ens.x \
        getsfcensmeanp.x getsigensmeanp_smooth.x getsigensstatp.x \
        interp_inc.x recentersigp.x; do
        safe_link_or_copy "${HOMEglobal}/sorc/gsi_utils.fd/install/bin/${exe}" .
    done
fi

# GSI Monitor
if [[ -d "${HOMEglobal}/sorc/gsi_monitor.fd/install" ]]; then
    for exe in oznmon_horiz.x oznmon_time.x radmon_angle.x \
        radmon_bcoef.x radmon_bcor.x radmon_time.x; do
        safe_link_or_copy "${HOMEglobal}/sorc/gsi_monitor.fd/install/bin/${exe}" .
    done
fi

# GDASApp executables
if [[ -d "${HOMEglobal}/sorc/gdas.cd/install" ]]; then
    cp -f "${HOMEglobal}/sorc/gdas.cd/install/bin"/gdas* ./
    cp -f "${HOMEglobal}/sorc/gdas.cd/install/bin/satbias2ioda.x" ./gdas_satbias2ioda.x
    cp -f "${HOMEglobal}/sorc/gdas.cd/install/bin/apply_incr.exe" ./gdas_apply_incr.x
fi

# GDASApp libraries
if [[ -d "${HOMEglobal}/sorc/gdas.cd/install" ]]; then
    mkdir -p "${HOMEglobal}/lib" || exit 1
    cd "${HOMEglobal}/lib" || exit 1
    cp -af "${HOMEglobal}/sorc/gdas.cd/install/lib/." ./
fi

# NEXUS executable
if [[ -d "${HOMEglobal}/sorc/nexus.fd/build/bin" ]]; then
    cd "${HOMEglobal}/exec" || exit 1
    safe_link_or_copy "${HOMEglobal}/sorc/nexus.fd/build/bin/nexus" nexus.x
fi

#------------------------------
#--link source code directories
#------------------------------
cd "${HOMEglobal}/sorc" || exit 8

if [[ -d gsi_enkf.fd ]]; then
    safe_link gsi_enkf.fd/src/gsi gsi.fd
    safe_link gsi_enkf.fd/src/enkf enkf.fd
fi

if [[ -d gsi_utils.fd ]]; then
    safe_link gsi_utils.fd/src/netcdf_io/calc_analysis.fd .
    safe_link gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens.fd .
    safe_link gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens_ncio.fd .
    safe_link gsi_utils.fd/src/EnKF/gfs/src/getsfcensmeanp.fd .
    safe_link gsi_utils.fd/src/EnKF/gfs/src/getsigensmeanp_smooth.fd .
    safe_link gsi_utils.fd/src/EnKF/gfs/src/getsigensstatp.fd .
    safe_link gsi_utils.fd/src/EnKF/gfs/src/recentersigp.fd .
    safe_link gsi_utils.fd/src/netcdf_io/interp_inc.fd .
fi

if [[ -d gsi_monitor.fd ]]; then
    safe_link gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_horiz.fd .
    safe_link gsi_monitor.fd/src/Ozone_Monitor/nwprod/oznmon_shared/sorc/oznmon_time.fd .
    safe_link gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radang.fd radmon_angle.fd
    safe_link gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcoef.fd radmon_bcoef.fd
    safe_link gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radbcor.fd radmon_bcor.fd
    safe_link gsi_monitor.fd/src/Radiance_Monitor/nwprod/radmon_shared/sorc/verf_radtime.fd radmon_time.fd
fi

if [[ -d ufs_model.fd ]]; then
    safe_link ufs_model.fd/WW3 WW3.fd
fi

for prog in chgres_cube.fd global_cycle.fd emcsfc_ice_blend.fd emcsfc_snow2mdl.fd; do
    safe_link "ufs_utils.fd/sorc/${prog}" "${prog}"
done

safe_link "ufs_utils.fd/sorc/regrid_sfc.fd" "regridStates.fd"

for prog in enkf_chgres_recenter_nc.fd \
    ensadd.fd \
    ensppf.fd \
    ensstat.fd \
    fbwndgfs.fd \
    gaussian_sfcanl.fd \
    gfs_bufr.fd \
    mkgfsawps.fd \
    ocnicepost.fd \
    overgridid.fd \
    rdbfmsua.fd \
    supvit.fd \
    syndat_getjtbul.fd \
    syndat_maksynrc.fd \
    syndat_qctropcy.fd \
    tave.fd \
    tocsbufr.fd \
    tref_calc.fd \
    vint.fd \
    wave_stat.fd \
    webtitle.fd; do
    safe_link_or_copy "gfs_utils.fd/src/${prog}" .
done

exit 0
