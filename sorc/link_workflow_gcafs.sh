#!/bin/bash

#--make symbolic links for EMC installation and hardcopies for NCO delivery

HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")" > /dev/null 2>&1 && git rev-parse --show-toplevel)
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
            RUN_ENVIR="nco"
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

# shellcheck disable=SC1091
COMPILER="intel" source "${HOMEgfs}/ush/detect_machine.sh" # (sets MACHINE_ID)
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
    "wcoss2") FIX_DIR="/lfs/h2/emc/global/noscrub/emc.global/FIX/fix" ;;
    "hera" | "ursa") FIX_DIR="/scratch3/NCEPDEV/global/role.glopara/fix" ;;
    "orion") FIX_DIR="/work2/noaa/global/role-global/fix" ;;
    "hercules") FIX_DIR="/work2/noaa/global/role-global/fix" ;;
    "gaeac5") FIX_DIR="/gpfs/f5/ufs-ard/world-shared/global/glopara/data/fix" ;;
    "gaeac6") FIX_DIR="/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix" ;;
    "noaacloud") FIX_DIR="/lustre/fix" ;;
    *)
        echo "FATAL: Unknown target machine ${machine}, couldn't set FIX_DIR"
        exit 1
        ;;
esac

# Source fix version file
source "${HOMEgfs}/versions/fix.ver"

# Link gdasapp python packages in ush/python
packages=("jcb")
for package in "${packages[@]}"; do
    cd "${HOMEgfs}/ush/python" || exit 1
    if [[ -s "${package}" ]]; then
        rm -f "${package}"
    fi
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/sorc/${package}/src/${package}" .
done

# Link fix directories
if [[ -n "${FIX_DIR}" ]]; then
    mkdir -p "${HOMEgfs}/fix" || exit 1
fi
cd "${HOMEgfs}/fix" || exit 1
for dir in aer \
    am \
    chem \
    cpl \
    gsi \
    lut \
    orog \
    sfc_climo \
    ugwd \
    verif; do
    if [[ -d "${dir}" ]]; then
        if [[ "${RUN_ENVIR}" == "nco" ]]; then
            chmod -R 755 "${dir}"
        fi
        rm -rf "${dir}"
    fi
    fix_ver="${dir}_ver"
    ${LINK_OR_COPY} "${FIX_DIR}/${dir}/${!fix_ver}" "${dir}"
done

#---------------------------------------
#--add files from external repositories
#---------------------------------------
#--copy/link NoahMp table form ccpp-physics repository
cd "${HOMEgfs}/parm/ufs" || exit 1
${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/parm/noahmptable.tbl" .

cd "${HOMEgfs}/parm/post" || exit 1
${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/parm/params_grib2_tbl_new" .
${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/fix/nam_micro_lookup.dat" .

${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/parm/gcafs" .

for file in optics_luts_DUST.dat optics_luts_DUST_nasa.dat optics_luts_NITR_nasa.dat \
    optics_luts_SALT.dat optics_luts_SALT_nasa.dat optics_luts_SOOT.dat optics_luts_SOOT_nasa.dat \
    optics_luts_SUSO.dat optics_luts_SUSO_nasa.dat optics_luts_WASO.dat optics_luts_WASO_nasa.dat; do
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/fix/chem/${file}" .
done

# Link these templates from ufs-weather-model
cd "${HOMEgfs}/parm/ufs" || exit 1
declare -a ufs_templates=("model_configure.IN"
    "ufs.configure.atm.IN"
    "ufs.configure.atmaero.IN"
    "post_itag_gcafs"
    "global_control.nml.IN")

for file in "${ufs_templates[@]}"; do
    if [[ -s "${file}" ]]; then
        rm -f "${file}"
    fi
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/parm/${file}" .
done

# Link the script from ufs-weather-model that parses the templates
cd "${HOMEgfs}/ush" || exit 1
if [[ -s "atparse.bash" ]]; then
    rm -f "atparse.bash"
fi
${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/atparse.bash" .

# add ufs_utils parm dir
if [[ -d "${HOMEgfs}/sorc/ufs_utils.fd" ]]; then
    cd "${HOMEgfs}/parm" || exit 1
    mkdir -p regrid_sfc
    cd regrid_sfc || exit 1
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_utils.fd/parm/regrid_sfc/regrid.nml_tmpl" .
fi

#------------------------------
#--add gdasApp fix directory
#------------------------------
if [[ -d "${HOMEgfs}/sorc/gdas.cd" ]]; then
    cd "${HOMEgfs}/fix" || exit 1
    mkdir -p gdas
    cd gdas || exit 1
    for gdas_sub in fv3jedi obs aero; do
        if [[ -d "${gdas_sub}" ]]; then
            rm -rf "${gdas_sub}"
        fi
        fix_ver="gdas_${gdas_sub}_ver"
        ${LINK_OR_COPY} "${FIX_DIR}/gdas/${gdas_sub}/${!fix_ver}" "${gdas_sub}"
    done
fi

#------------------------------
#--add gdasApp parm directory
#------------------------------
if [[ -d "${HOMEgfs}/sorc/gdas.cd" ]]; then
    cd "${HOMEgfs}/parm" || exit 1
    mkdir -p gdas
    cd gdas || exit 1
    declare -a gdasapp_comps=("aero" "atm" "io" "ioda" "jcb-gdas" "jcb-algorithms" "anlstat" "analcalc")
    for comp in "${gdasapp_comps[@]}"; do
        if [[ -d "${comp}" ]]; then
            rm -rf "${comp}"
        fi
        ${LINK_OR_COPY} "${HOMEgfs}/sorc/gdas.cd/parm/${comp}" .
    done
fi

#------------------------------
#--add NEXUS files
#------------------------------
if [[ -d "${HOMEgfs}/sorc/nexus.fd" ]]; then
    cd "${HOMEgfs}/parm/chem" || exit 1
    if [[ -d nexus ]]; then
        rm -rf nexus
    fi
    mkdir -p nexus/gocart
    cd nexus/gocart || exit 1
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/nexus.fd/config/gocart/NEXUS_Config.rc.j2" .
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/nexus.fd/config/gocart/HEMCO_sa_Grid.rc.j2" .
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/nexus.fd/config/gocart/HEMCO_sa_Time.rc.j2" .
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/nexus.fd/config/gocart/HEMCO_sa_Diag.rc.j2" .
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/nexus.fd/config/gocart/HEMCO_sa_Spec.rc.j2" .
fi

#------------------------------
#--link executables
#------------------------------

mkdir -p "${HOMEgfs}/exec" || exit 1

cd "${HOMEgfs}/exec" || exit 1

for utilexe in gaussian_sfcanl.x enkf_chgres_recenter_nc.x tref_calc.x; do
    if [[ -s "${utilexe}" ]]; then
        rm -f "${utilexe}"
    fi
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/gfs_utils.fd/install/bin/${utilexe}" .
done

declare -a model_systems=("gcafs")
for sys in "${model_systems[@]}"; do
    model_exe="${sys}_model.x"
    if [[ -s "${model_exe}" ]]; then
        rm -f "${model_exe}"
    fi
    if [[ -f "${HOMEgfs}/sorc/ufs_model.fd/tests/${model_exe}" ]]; then
        ${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_model.fd/tests/${model_exe}" "${model_exe}"
    fi
done

if [[ -s "upp.x" ]]; then
    rm -f upp.x
fi
${LINK_OR_COPY} "${HOMEgfs}/sorc/upp.fd/exec/upp.x" .

for ufs_utilsexe in emcsfc_ice_blend emcsfc_snow2mdl global_cycle; do
    if [[ -s "${ufs_utilsexe}" ]]; then
        rm -f "${ufs_utilsexe}"
    fi
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/ufs_utils.fd/exec/${ufs_utilsexe}" .
done

# GSI Utils
if [[ -d "${HOMEgfs}/sorc/gsi_utils.fd/install" ]]; then
    for exe in calc_analysis.x calc_increment_ens_ncio.x \
        interp_inc.x; do
        if [[ -s "${exe}" ]]; then
            rm -f "${exe}"
        fi
        ${LINK_OR_COPY} "${HOMEgfs}/sorc/gsi_utils.fd/install/bin/${exe}" .
    done
fi

# gdasApp executables
if [[ -d "${HOMEgfs}/sorc/gdas.cd/install" ]]; then
    cp -f "${HOMEgfs}/sorc/gdas.cd/install/bin"/gdas* ./
fi

# gdasApp libraries
if [[ -d "${HOMEgfs}/sorc/gdas.cd/install" ]]; then
    mkdir -p "${HOMEgfs}/lib" || exit 1
    cd "${HOMEgfs}/lib" || exit 1
    cp -af "${HOMEgfs}/sorc/gdas.cd/install/lib/." ./
fi

# NEXUS executable
if [[ -d "${HOMEgfs}/sorc/nexus.fd/build/bin" ]]; then
    cd "${HOMEgfs}/exec" || exit 1
    ${LINK_OR_COPY} "${HOMEgfs}/sorc/nexus.fd/build/bin/nexus" nexus.x
fi

#------------------------------
#--link source code directories
#------------------------------
cd "${HOMEgfs}/sorc" || exit 8
if [[ -d ufs_model.fd ]]; then
    if [[ -d upp.fd ]]; then
        rm -rf upp.fd
    fi
    ${LINK} ufs_model.fd/UFSATM/upp upp.fd
fi

if [[ -d gsi_utils.fd ]]; then
    if [[ -d calc_analysis.fd ]]; then
        rm -rf calc_analysis.fd
    fi
    ${LINK} gsi_utils.fd/src/netcdf_io/calc_analysis.fd .

    if [[ -d calc_increment_ens_ncio.fd ]]; then
        rm -rf calc_increment_ens_ncio.fd
    fi
    ${LINK} gsi_utils.fd/src/EnKF/gfs/src/calc_increment_ens_ncio.fd .

    if [[ -d interp_inc.fd ]]; then
        rm -rf interp_inc.fd
    fi
    ${LINK} gsi_utils.fd/src/netcdf_io/interp_inc.fd .
fi

for prog in global_cycle.fd emcsfc_ice_blend.fd emcsfc_snow2mdl.fd; do
    if [[ -d "${prog}" ]]; then
        rm -rf "${prog}"
    fi
    ${LINK} "ufs_utils.fd/sorc/${prog}" "${prog}"
done

for prog in enkf_chgres_recenter_nc.fd \
    gaussian_sfcanl.fd \
    tref_calc.fd; do
    if [[ -d "${prog}" ]]; then rm -rf "${prog}"; fi
    ${LINK_OR_COPY} "gfs_utils.fd/src/${prog}" .
done

exit 0
