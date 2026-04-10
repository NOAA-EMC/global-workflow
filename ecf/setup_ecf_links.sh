#!/bin/bash
# Must be run from $PACKAGEHOME/ecf
set -eux
module load prod_util

# GFS ecflow workflow resource configuration
resource_config="EMC"
ECF_DIR=$(pwd)

# Create tmp file for git exclude
tmp_exclude="${ECF_DIR}/exclude_list.tmp"

# Function that loops over forecast hours and
# creates link between the master and target
function link_master_to_fhr() {
    tmpl=${1} # Name of the master template
    fhrs=${2} # Array of forecast hours
    for fhr in ${fhrs[@]}; do
        fhrchar=$(printf %03d "${fhr}")
        master="${tmpl}_master.ecf"
        target="${tmpl}_f${fhrchar}.ecf"
        rm -f "${target}"
        ln -sf "${master}" "${target}"
    done
}

# $1: The value to replace the placeholder with (e.g., "006_15").
# $2: The name of the output file to create.
create_ecf_file() {
    local placeholder_value="${1}"
    local output_filename="${2}"
    echo "Creating ${output_filename}..."
    sed "s|@ecf_fhr@|${placeholder_value}|g" "${MASTER_FILE}" > "${output_filename}"
}

add_to_tmpfile() {
    local exclude_file="${1}"
    echo "${exclude_file}" >> "${tmp_exclude}"
    echo "Added ${exclude_file} to ${tmp_exclude}"
}

################################################################################################
################################################################################################
# gfs wave postsbs files
cd "${ECF_DIR}/scripts/gfs/post/wave"
echo "Copy gfs wave postsbs files ..."
rm -f jgfs_wave_postsbs_f*.ecf
fhr_end=384
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgfs_wave_postsbs_master.ecf "jgfs_wave_postsbs_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 3))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/post/wave/jgfs_wave_postsbs_f${head_3d}.ecf"
done

# gfs atmos product files
cd "${ECF_DIR}/scripts/gfs/product/atmos"
echo "Copy gfs atmos product files ..."
rm -f jgfs_atmos_product_f*.ecf
fhr_end=384
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgfs_atmos_product_master.ecf "jgfs_atmos_product_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 3))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/product/atmos/jgfs_atmos_product_f${head_3d}.ecf"
done

# gfs ocean product files
cd "${ECF_DIR}/scripts/gfs/product/ocean"
echo "Copy gfs ocean product files ..."
rm -f jgfs_ocean_product_f*.ecf
fhr_end=384
fhr_start=6
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start})"
    cp jgfs_ocean_product_master.ecf "jgfs_ocean_product_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 6))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/product/ocean/jgfs_ocean_product_f${head_3d}.ecf"
done

# gfs ice product files
cd "${ECF_DIR}/scripts/gfs/product/ice"
echo "Copy gfs ice product files ..."
rm -f jgfs_ice_product_f*.ecf
fhr_end=384
fhr_start=6
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start})"
    cp jgfs_ice_product_master.ecf "jgfs_ice_product_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 6))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/product/ice/jgfs_ice_product_f${head_3d}.ecf"
done

# gdas atmos product files
cd "${ECF_DIR}/scripts/gdas/product/atmos"
echo "Copy gdas atmos product files ..."
rm -f jgdas_atmos_product_f???.ecf
fhr_end=9
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    step=1
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgdas_atmos_product_master.ecf "jgdas_atmos_product_f${head_3d}.ecf"
    fhr_start=$((fhr_start + step))
    add_to_tmpfile "${ECF_DIR}/scripts/gdas/product/atmos/jgdas_atmos_product_f${head_3d}.ecf"
done

# gdas wave postsbs files
cd "${ECF_DIR}/scripts/gdas/post/wave"
echo "Copy gdas wave postsbs files ..."
rm -f jgdas_wave_postsbs_f???.ecf
fhr_end=9
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    step=1
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgdas_wave_postsbs_master.ecf "jgdas_wave_postsbs_f${head_3d}.ecf"
    fhr_start=$((fhr_start + step))
    add_to_tmpfile "${ECF_DIR}/scripts/gdas/post/wave/jgfs_wave_postsbs_f${head_3d}.ecf"
done

# enkfgdas ecen files
cd "${ECF_DIR}/scripts/enkfgdas/analysis/recenter/ecen"
echo "Copy enkfgdas ecen files ..."
rm -f jenkfgdas_ecen00?.ecf
fhr_end=2
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    step=1
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jenkfgdas_ecen_master.ecf "jenkfgdas_ecen${head_3d}.ecf"
    fhr_start=$((fhr_start + step))
    add_to_tmpfile "${ECF_DIR}/scripts/enkfgdas/analysis/recenter/ecen/jenkfgdas_ecen${head_3d}.ecf"
done

# enkfgdas fcst files
cd "${ECF_DIR}/scripts/enkfgdas/forecast"
echo "Copy enkfgdas fcst files ..."
rm -f jenkfgdas_fcst_mem0??.ecf
fhr_end=80
fhr_start=1
while [ ${fhr_start} -le ${fhr_end} ]; do
    step=1
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jenkfgdas_fcst_master.ecf "jenkfgdas_fcst_mem${head_3d}.ecf"
    fhr_start=$((fhr_start + step))
    add_to_tmpfile "${ECF_DIR}/scripts/enkfgdas/forecast/jenkfgdas_fcst_mem${head_3d}.ecf"
done

# enkfgdas post files
cd "${ECF_DIR}/scripts/enkfgdas/post"
echo "Copy enkfgdas post files ..."
rm -f jenkfgdas_epos0??.ecf
fhr_end=6
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    step=1
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jenkfgdas_epos_master.ecf "jenkfgdas_epos${head_3d}.ecf"
    fhr_start=$((fhr_start + step))
    add_to_tmpfile "${ECF_DIR}/scripts/enkfgdas/post/jenkfgdas_epos${head_3d}.ecf"
done

# gfs atmos gempak files
cd "${ECF_DIR}/scripts/gfs/gempak/atmos/gempak"
echo "Copy gfs atmos gempak files ..."
rm -f jgfs_atmos_gempak_f*.ecf
fhr_end=384
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgfs_atmos_gempak_master.ecf "jgfs_atmos_gempak_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 3))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/gempak/atmos/gempak/jgfs_atmos_gempak_f${head_3d}.ecf"
done

# gfs weav gempak files
cd "${ECF_DIR}/scripts/gfs/gempak/wave"
echo "Copy gfs weav gempak files ..."
rm -f jgfs_wave_gempak_f*.ecf
fhr_end=180
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    step=3
    [[ ${fhr_start} -ge 72 ]] && step=6
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgfs_wave_gempak_master.ecf "jgfs_wave_gempak_f${head_3d}.ecf"
    fhr_start=$((fhr_start + step))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/gempak/wave/jgfs_wave_gempak_f${head_3d}.ecf"
done

# gdas atmos gempak files
cd "${ECF_DIR}/scripts/gdas/gempak/atmos/gempak"
echo "Copy gfs atmos gempak files ..."
rm -f jgdas_atmos_gempak_f*.ecf
fhr_end=9
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgdas_atmos_gempak_master.ecf "jgdas_atmos_gempak_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 1))
    add_to_tmpfile "${ECF_DIR}/scripts/gdas/gempak/atmos/gempak/jgdas_atmos_gempak_f${head_3d}.ecf"
done

# gfs atmos goesupp files
cd "${ECF_DIR}/scripts/gfs/gempak/atmos/goesupp"
echo "Copy gfs atmos goesupp files ..."
rm -f jgfs_atmos_goesupp_f*.ecf
fhr_end=384
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgfs_atmos_goesupp_master.ecf "jgfs_atmos_goesupp_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 3))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/gempak/atmos/goesupp/jgfs_atmos_goesupp_f${head_3d}.ecf"
done

# gfs atmos 20km 1p0 files
cd "${ECF_DIR}/scripts/gfs/product/atmos/awips_20km_1p0"
echo "Copy gfs atmos 20km 1p0 files ..."
rm -f jgfs_atmos_awips_20km_1p0_f*.ecf
fhr_end=240
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgfs_atmos_awips_20km_1p0_master.ecf "jgfs_atmos_awips_20km_1p0_f${head_3d}.ecf"
    if [ ${fhr_start} -ge 84 ]; then
        fhr_start=$((fhr_start + 6))
    else
        fhr_start=$((fhr_start + 3))
    fi
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/product/atmos/awips_20km_1p0/jgfs_atmos_awips_20km_1p0_f${head_3d}.ecf"
done

# gfs atmos grb2spec files
cd "${ECF_DIR}/scripts/gfs/gempak/atmos/grb2spec"
echo "Copy gfs atmos grb2spec files ..."
rm -f jgfs_atmos_grb2spec_f*.ecf
fhr_end=180
fhr_start=0
while [ ${fhr_start} -le ${fhr_end} ]; do
    head_3d=$(printf "%03d" "${fhr_start}")
    cp jgfs_atmos_grb2spec_master.ecf "jgfs_atmos_grb2spec_f${head_3d}.ecf"
    fhr_start=$((fhr_start + 3))
    add_to_tmpfile "${ECF_DIR}/scripts/gfs/gempak/atmos/grb2spec/jgfs_atmos_grb2spec_f${head_3d}.ecf"
done
