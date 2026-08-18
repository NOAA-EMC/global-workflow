#!/bin/bash
#
# test_module_removal.sh — Test which modules can be safely removed
#
# Method: Load ALL modules, then try unloading each one individually
# and check if the target binary still resolves all shared libraries.
#
# Usage:
#   source ${HOMEgfs}/versions/run.ver
#   ./test_module_removal.sh /path/to/binary
#
# Example:
#   ./test_module_removal.sh ${HOMEgfs}/exec/upp.x
#
set -u

BINARY="${1:-}"
if [[ -z "${BINARY}" || ! -f "${BINARY}" ]]; then
    echo "Usage: $0 <path-to-binary>"
    echo "Example: $0 \${HOMEgfs}/exec/upp.x"
    exit 1
fi

echo "=============================================="
echo " Module Removal Test"
echo " Binary: ${BINARY}"
echo "=============================================="
echo ""

# The full module set (from gw_run.wcoss2.lua order)
FULL_MODS=(
    "PrgEnv-intel/${PrgEnv_intel_ver:-8.5.0}"
    "craype/${craype_ver:-2.7.17}"
    "intel/${intel_ver:-19.1.3.304}"
    "cray-mpich/${cray_mpich_ver:-8.1.19}"
    "cray-pals/${cray_pals_ver:-1.3.2}"
    "cfp/${cfp_ver:-2.0.4}"
    "python/${python_ver:-3.12.0}"
    "ve/gfs/${ve_gfs_ver:-17.0}"
    "gempak/${gempak_ver:-7.15.1}"
    "perl/${perl_ver:-5.32.0}"
    "libjpeg/${libjpeg_ver:-9c}"
    "udunits/${udunits_ver:-2.2.28}"
    "gsl/${gsl_ver:-2.7}"
    "cdo/${cdo_ver:-2.0.5}"
    "imagemagick/${imagemagick_ver:-7.0.8-7}"
    "hdf5-D/${hdf5_ver:-1.14.0}"
    "pnetcdf-D/${pnetcdf_ver:-1.12.3}"
    "netcdf-D/${netcdf_ver:-4.9.2}"
    "esmf-D/${esmf_ver:-8.6.0}"
    "nco/${nco_ver:-5.0.6}"
    "grib_util/${grib_util_ver:-1.2.4}"
    "bufr_dump/${bufr_dump_ver:-2.0.0}"
    "util_shared/${util_shared_ver:-1.4.0}"
    "g2tmpl/${g2tmpl_ver:-1.16.0}"
    "ncdiag-A/${ncdiag_ver:-1.1.2}"
    "crtm/${crtm_ver:-2.4.0.1}"
    "wgrib2/${wgrib2_ver:-2.0.8_wmo}"
)

# Modules that cannot be removed (hierarchy/infrastructure)
SKIP_REMOVE="PrgEnv-intel craype intel"

echo "[1/3] Loading all ${#FULL_MODS[@]} modules..."
module purge 2>/dev/null || true
module reset 2>/dev/null || true

for mod in "${FULL_MODS[@]}"; do
    module load "${mod}" 2>/dev/null
done

echo "  Loaded $(module -t list 2>&1 | grep -cv '^$\|Currently' || true) modules"
echo ""

# Baseline check
baseline_missing=$(ldd "${BINARY}" 2>/dev/null | grep "not found" || true)
if [[ -n "${baseline_missing}" ]]; then
    echo "[ERROR] Even with ALL modules loaded, binary has missing libs:"
    echo "${baseline_missing}"
    echo "Cannot proceed."
    exit 1
fi
echo "[2/3] Baseline OK — all libraries resolve with full module set."
echo ""

echo "[3/3] Testing removal of each module..."
echo ""
printf "  %-30s %s\n" "MODULE" "RESULT"
printf "  %-30s %s\n" "------" "------"

NEEDED=()
NOT_NEEDED=()

for mod in "${FULL_MODS[@]}"; do
    mod_base="${mod%%/*}"

    # Skip infrastructure modules that can't be removed
    skip="NO"
    for s in ${SKIP_REMOVE}; do
        if [[ "${mod_base}" == "${s}" ]]; then skip="YES"; break; fi
    done
    if [[ "${skip}" == "YES" ]]; then
        printf "  %-30s %s\n" "${mod}" "SKIP (infrastructure)"
        NEEDED+=("${mod}")
        continue
    fi

    # Try unloading this module
    module unload "${mod_base}" 2>/dev/null

    # Check if binary still resolves
    missing=$(ldd "${BINARY}" 2>/dev/null | grep "not found" || true)

    if [[ -n "${missing}" ]]; then
        # Needed — reload it
        module load "${mod}" 2>/dev/null
        printf "  %-30s %s\n" "${mod}" "NEEDED (libs break without it)"
        NEEDED+=("${mod}")
    else
        # Not needed — leave it unloaded
        printf "  %-30s %s\n" "${mod}" "NOT NEEDED"
        NOT_NEEDED+=("${mod}")
    fi
done

echo ""
echo "=============================================="
echo " RESULTS"
echo "=============================================="
echo ""
echo " NEEDED (${#NEEDED[@]} modules):"
for mod in "${NEEDED[@]}"; do
    mod_base="${mod%%/*}"
    # Handle multi-slash (ve/gfs)
    slashes="${mod//[^\/]/}"
    if [[ ${#slashes} -ge 2 ]]; then
        mod_base="${mod%/*}"
    fi
    ver_var="${mod_base//\//_}"
    ver_var="${ver_var//-/_}"
    ver_var="${ver_var%_D}"
    ver_var="${ver_var%_A}"
    ver_var="${ver_var}_ver"
    echo "   module load ${mod_base}/\${${ver_var}}"
done

echo ""
echo " NOT NEEDED (${#NOT_NEEDED[@]} modules — safe to remove):"
for mod in "${NOT_NEEDED[@]}"; do
    echo "   ${mod}"
done

echo ""
echo " Reduction: ${#FULL_MODS[@]} → ${#NEEDED[@]} modules (removed ${#NOT_NEEDED[@]})"
echo "=============================================="
