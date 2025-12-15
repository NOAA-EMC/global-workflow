#!/bin/bash
#shellcheck disable=SC2207
PS4='+ $LINENO: '
set -eux

# USE AT YOUR OWN RISK!  This script is provided as-is without support and is not meant to be
# a general-purpose solution.  It has only been tested on a limited set of data and directory structures.

# This script converts filenames from the older (v16+) naming convention to EE2-compatible names by creating symbolic links.
#
# Usage: make_ee2_links.sh <target_directory>
# Conversions are available for gdas, gfs, and enkfgdas (but not enkfgfs at this time).
#
# WARNING: This script does not create all links needed for EE2 compatibility. It only creates links needed to
#          restart an existing experiment.

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <target_directory>"
    exit 1
fi

target_dir=$1
cd "${target_dir}" || exit 1

# A helper function to create symbolic links with error checking
link_file() {
    if [[ $# -ne 2 ]]; then
        echo "Error: link_file requires exactly 2 arguments: target and link."
        return 1
    fi
    local target=$1
    local link=$2
    if [[ ! -f "${target}" ]]; then
        echo "Error: Target file ${target} does not exist."
        return 1
    fi
    if [[ -f "${link}" && ! -L "${link}" ]]; then
        echo "Error: Targeted link ${link} exists and is not a symbolic link. Not overwriting data files."
        return 1
    fi
    ln -sf "${target}" "${link}"
    return 0
}

gdas_list=($(ls -d gdas.* || true))
gfs_list=($(ls -d gfs.* || true))
gcdas_list=($(ls -d gcdas.* || true))
gcafs_list=($(ls -d gcafs.* || true))
enkfgdas_list=($(ls -d enkfgdas.* || true))
enkfgfs_list=($(ls -d enkfgfs.* || true))
enkfgcdas_list=($(ls -d enkfgcdas.* || true))

# If the length of all of the arrays is zero, exit with a message
if [[ ${#gdas_list[@]} -eq 0 && ${#gfs_list[@]} -eq 0 && ${#gcdas_list[@]} -eq 0 &&
    ${#gcafs_list[@]} -eq 0 && ${#enkfgdas_list[@]} -eq 0 &&
    ${#enkfgfs_list[@]} -eq 0 && ${#enkfgcdas_list[@]} -eq 0 ]]; then
    echo "No gdas, gfs, gcdas, gcafs, enkfgdas, enkfgfs, or enkfgcdas directories found. Exiting."
    exit 0
fi

cwd=${PWD}
# Loop through the gdas, gfs, gcdas, and gcafs directories
for dir in "${gdas_list[@]}" "${gfs_list[@]}" "${gcdas_list[@]}" "${gcafs_list[@]}"; do
    cd "${dir}"
    # Determine the system prefix
    system_prefix=""
    case "${dir}" in
        gdas.*) system_prefix="gdas" ;;
        gfs.*) system_prefix="gfs" ;;
        gcdas.*) system_prefix="gcdas" ;;
        gcafs.*) system_prefix="gcafs" ;;
        *)
            echo "Unknown directory prefix: ${dir}"
            exit 1
            ;;
    esac

    cycle_list=($(ls -d -- ?? || true))
    for cyc in "${cycle_list[@]}"; do
        if [[ -d "${cwd}/${dir}/${cyc}/analysis/atmos" ]]; then
            cd "${cwd}/${dir}/${cyc}/analysis/atmos"
            for abias_type in abias abias_air abias_int abias_pc; do
                if [[ -f "${system_prefix}.t${cyc}z.${abias_type}" ]]; then
                    link_file "${system_prefix}.t${cyc}z.${abias_type}" "${system_prefix}.t${cyc}z.${abias_type}.txt"
                fi
            done
            if [[ -f "${system_prefix}.t${cyc}z.radstat" ]]; then
                link_file "${system_prefix}.t${cyc}z.radstat" "${system_prefix}.t${cyc}z.radstat.tar"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.atmi003.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.atmi003.nc" "${system_prefix}.t${cyc}z.increment.atm.i003.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.atminc.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.atminc.nc" "${system_prefix}.t${cyc}z.increment.atm.i006.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.atmi009.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.atmi009.nc" "${system_prefix}.t${cyc}z.increment.atm.i009.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.atma003.ensres.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.atma003.ensres.nc" "${system_prefix}.t${cyc}z.ensres_analysis.atm.a003.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.atmanl.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.atmanl.nc" "${system_prefix}.t${cyc}z.analysis.atm.a006.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.atmanl.ensres.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.atmanl.ensres.nc" "${system_prefix}.t${cyc}z.ensres_analysis.atm.a006.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.atma009.ensres.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.atma009.ensres.nc" "${system_prefix}.t${cyc}z.ensres_analysis.atm.a009.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.cnvstat" ]]; then
                link_file "${system_prefix}.t${cyc}z.cnvstat" "${system_prefix}.t${cyc}z.cnvstat.tar"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.dtfanl.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.dtfanl.nc" "${system_prefix}.t${cyc}z.analysis.dtf.a006.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.gsistat" ]]; then
                link_file "${system_prefix}.t${cyc}z.gsistat" "${system_prefix}.t${cyc}z.gsistat.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.oznstat" ]]; then
                link_file "${system_prefix}.t${cyc}z.oznstat" "${system_prefix}.t${cyc}z.oznstat.tar"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.loginc.txt" ]]; then
                link_file "${system_prefix}.t${cyc}z.loginc.txt" "${system_prefix}.t${cyc}z.increment.done.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.loganl.txt" ]]; then
                link_file "${system_prefix}.t${cyc}z.loganl.txt" "${system_prefix}.t${cyc}z.analysis.done.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.sfci003.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfci003.nc" "${system_prefix}.t${cyc}z.increment.sfc.i003.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.sfci006.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfci006.nc" "${system_prefix}.t${cyc}z.increment.sfc.i006.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.sfci009.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfci009.nc" "${system_prefix}.t${cyc}z.increment.sfc.i009.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.sfcanl.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfcanl.nc" "${system_prefix}.t${cyc}z.analysis.sfc.a006.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.sfcinc.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfcinc.nc" "${system_prefix}.t${cyc}z.increment.sfc.i006.nc"
            fi
            if [[ -f "sfc_inc.tile1.nc" ]]; then
                for tile in {1..6}; do
                    link_file "sfc_inc.tile${tile}.nc" "increment.sfc.tile${tile}.nc"
                done
            fi
            if [[ -f "${system_prefix}.t${cyc}z.cubed_sphere_grid_atminc.tile1.nc" ]]; then
                for tile in {1..6}; do
                    link_file "${system_prefix}.t${cyc}z.cubed_sphere_grid_atminc.tile${tile}.nc" "${system_prefix}.t${cyc}z.jedi_increment.atm.i006.tile${tile}.nc"
                done
            fi
        fi
        cd "${cwd}"
        if [[ -d "${cwd}/${dir}/${cyc}/analysis/ocean" ]]; then
            cd "${cwd}/${dir}/${cyc}/analysis/ocean"
            if [[ -f "${system_prefix}.t${cyc}z.ocninc.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.ocninc.nc" "${system_prefix}.t${cyc}z.mom6_increment.i006.nc"
            fi
        fi
        cd "${cwd}"
        if [[ -d "${cwd}/${dir}/${cyc}/analysis/ice" ]]; then
            cd "${cwd}/${dir}/${cyc}/analysis/ice"
            for ice_file in *.cice_model_anl.res.nc; do
                if [[ -f "${ice_file}" ]]; then
                    # This gets the first two fields of the filename separated by dots
                    prefix=$(echo "${ice_file}" | cut -d. -f1-2)
                    link_file "${ice_file}" "${prefix}.analysis.cice_model.res.nc"
                fi
            done
        fi
        cd "${cwd}"
        if [[ -d "${cwd}/${dir}/${cyc}/analysis/snow" ]]; then
            cd "${cwd}/${dir}/${cyc}/analysis/snow"
            for snow_file in snowinc*.sfc_data.tile1.nc; do
                if [[ -f "${snow_file}" ]]; then
                    prefix=$(echo "${snow_file}" | cut -d. -f1-3)
                    # Keep the date fields of the prefix (the last two fields)
                    # Lop off the "snowinc."
                    prefix_new=$(echo "${prefix}" | cut -d. -f2-)
                    for tile in {1..6}; do
                        link_file "${prefix}.sfc_data.tile${tile}.nc" "${prefix_new}.snow_increment.sfc_data.tile${tile}.nc"
                    done
                fi
            done
        fi
    done
    cd "${cwd}"
done

for dir in "${enkfgdas_list[@]}" "${enkfgfs_list[@]}"; do
    cd "${dir}"
    cycle_list=($(ls -d -- ?? || true))
    # Determine the system prefix
    system_prefix=""
    case "${dir}" in
        enkfgdas.*) system_prefix="enkfgdas" ;;
        enkfgfs.*) system_prefix="enkfgfs" ;;
        enkfgcdas.*) system_prefix="enkfgcdas" ;;
        *)
            echo "Unknown directory prefix: ${dir}"
            exit 1
            ;;
    esac
    for cyc in "${cycle_list[@]}"; do
        cd "${cwd}/${dir}/${cyc}"
        mem_list=($(ls -d mem* || true))
        for mem in "${mem_list[@]}"; do
            # atmos
            if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/atmos" ]]; then
                cd "${cwd}/${dir}/${cyc}/${mem}/analysis/atmos"
                if [[ -f "${system_prefix}.t${cyc}z.atmi003.nc" ]]; then
                    link_file "${system_prefix}.t${cyc}z.atmi003.nc" "${system_prefix}.t${cyc}z.increment.atm.i003.nc"
                fi
                if [[ -f "${system_prefix}.t${cyc}z.atminc.nc" ]]; then
                    link_file "${system_prefix}.t${cyc}z.atminc.nc" "${system_prefix}.t${cyc}z.increment.atm.i006.nc"
                fi
                if [[ -f "${system_prefix}.t${cyc}z.atmi009.nc" ]]; then
                    link_file "${system_prefix}.t${cyc}z.atmi009.nc" "${system_prefix}.t${cyc}z.increment.atm.i009.nc"
                fi
                # Handle recentered increments
                if [[ -f "${system_prefix}.t${cyc}z.ratmi003.nc" ]]; then
                    link_file "${system_prefix}.t${cyc}z.ratmi003.nc" "${system_prefix}.t${cyc}z.recentered_increment.atm.i003.nc"
                fi
                if [[ -f "${system_prefix}.t${cyc}z.ratminc.nc" ]]; then
                    link_file "${system_prefix}.t${cyc}z.ratminc.nc" "${system_prefix}.t${cyc}z.recentered_increment.atm.i006.nc"
                fi
                if [[ -f "${system_prefix}.t${cyc}z.ratmi009.nc" ]]; then
                    link_file "${system_prefix}.t${cyc}z.ratmi009.nc" "${system_prefix}.t${cyc}z.recentered_increment.atm.i009.nc"
                fi
                # Recentered jedi increments
                if [[ -f "${system_prefix}.t${cyc}z.cubed_sphere_grid_ratminc.tile1.nc" ]]; then
                    for tile in {1..6}; do
                        link_file "${system_prefix}.t${cyc}z.cubed_sphere_grid_ratminc.tile${tile}.nc" "${system_prefix}.t${cyc}z.recentered_jedi_increment.atm.i006.tile${tile}.nc"
                    done
                fi
                # abias
                for abias_type in abias abias_air abias_int abias_nc; do
                    if [[ -f "${system_prefix}.t${cyc}z.${abias_type}.ensmean" ]]; then
                        link_file "${system_prefix}.t${cyc}z.${abias_type}.ensmean" "${system_prefix}.t${cyc}z.${abias_type}.ensmean.txt"
                    fi
                done
                # stats
                for stat_type in cnvstat gsistat oznstat radstat; do
                    if [[ -f "${system_prefix}.t${cyc}z.${stat_type}.ensmean" ]]; then
                        link_file "${system_prefix}.t${cyc}z.${stat_type}.ensmean" "${system_prefix}.t${cyc}z.${stat_type}.ensmean.tar"
                    fi
                done
                if [[ -f "${system_prefix}.t${cyc}z.enkfstat" ]]; then
                    link_file "${system_prefix}.t${cyc}z.enkfstat" "${system_prefix}.t${cyc}z.enkfstat.txt"
                fi
                # surface increments
                for inc_time in 003 006 009; do
                    if [[ -f "${system_prefix}.t${cyc}z.sfci${inc_time}.nc" ]]; then
                        link_file "${system_prefix}.t${cyc}z.sfci${inc_time}.nc" "${system_prefix}.t${cyc}z.increment.sfc.i${inc_time}.nc"
                    fi
                done
                # sfc_inc tile links
                if [[ -f "sfc_inc.tile1.nc" ]]; then
                    for tile in {1..6}; do
                        link_file "sfc_inc.tile${tile}.nc" "increment.sfc.tile${tile}.nc"
                    done
                fi
            fi
            # ocean
            if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/ocean" ]]; then
                cd "${cwd}/${dir}/${cyc}/${mem}/analysis/ocean"
                if [[ -f "${system_prefix}.t${cyc}z.ocninc.nc" ]]; then
                    link_file "${system_prefix}.t${cyc}z.ocninc.nc" "${system_prefix}.t${cyc}z.mom6_increment.i006.nc"
                fi
            fi
            # ice
            if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/ice" ]]; then
                cd "${cwd}/${dir}/${cyc}/${mem}/analysis/ice"
                for ice_file in *.cice_model_anl.res.nc; do
                    if [[ -f "${ice_file}" ]]; then
                        prefix=$(echo "${ice_file}" | cut -d. -f1-2)
                        link_file "${ice_file}" "${prefix}.analysis.cice_model.res.nc"
                    fi
                done
            fi
            # snow
            if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/snow" ]]; then
                cd "${cwd}/${dir}/${cyc}/${mem}/analysis/snow"
                # The old snow analysis files start with YYYYMMDD.HHMMSS, which we want to keep
                for snow_file in ????????.??????.sfc_data.tile1.nc; do
                    if [[ -f "${snow_file}" ]]; then
                        prefix=$(echo "${snow_file}" | cut -d. -f1-2)
                        for tile in {1..6}; do
                            link_file "${prefix}.sfc_data.tile${tile}.nc" "${prefix}.snow_analysis.sfc_data.tile${tile}.nc"
                        done
                    fi
                done
            fi
            cd "${cwd}"
        done
        # ensstat files (no mem subdir)
        if [[ -d "${cwd}/${dir}/${cyc}/ensstat/analysis/atmos" ]]; then
            cd "${cwd}/${dir}/${cyc}/ensstat/analysis/atmos"
            if [[ -f "${system_prefix}.t${cyc}z.abias.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.abias.ensmean" "${system_prefix}.t${cyc}z.abias.ensmean.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.abias_air.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.abias_air.ensmean" "${system_prefix}.t${cyc}z.abias_air.ensmean.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.abias_int.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.abias_int.ensmean" "${system_prefix}.t${cyc}z.abias_int.ensmean.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.abias_pc.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.abias_pc.ensmean" "${system_prefix}.t${cyc}z.abias_pc.ensmean.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.cnvstat.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.cnvstat.ensmean" "${system_prefix}.t${cyc}z.cnvstat.ensmean.tar"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.enkfstat" ]]; then
                link_file "${system_prefix}.t${cyc}z.enkfstat" "${system_prefix}.t${cyc}z.enkfstat.txt"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.gsistat.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.gsistat.ensmean" "${system_prefix}.t${cyc}z.gsistat.ensmean.tar"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.oznstat.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.oznstat.ensmean" "${system_prefix}.t${cyc}z.oznstat.ensmean.tar"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.radstat.ensmean" ]]; then
                link_file "${system_prefix}.t${cyc}z.radstat.ensmean" "${system_prefix}.t${cyc}z.radstat.ensmean.tar"
            fi
            # Surface increments
            if [[ -f "${system_prefix}.t${cyc}z.sfci003.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfci003.nc" "${system_prefix}.t${cyc}z.increment.sfc.i003.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.sfci006.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfci006.nc" "${system_prefix}.t${cyc}z.increment.sfc.i006.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.sfci009.nc" ]]; then
                link_file "${system_prefix}.t${cyc}z.sfci009.nc" "${system_prefix}.t${cyc}z.increment.sfc.i009.nc"
            fi
            if [[ -f "${system_prefix}.t${cyc}z.loginc.txt" ]]; then
                link_file "${system_prefix}.t${cyc}z.loginc.txt" "${system_prefix}.t${cyc}z.increment.done.txt"
            fi
        fi
        # snow ensstat
        if [[ -d "${cwd}/${dir}/${cyc}/ensstat/analysis/snow" ]]; then
            cd "${cwd}/${dir}/${cyc}/ensstat/analysis/snow"
            for snow_file in snowinc*.sfc_data.tile1.nc; do
                if [[ -f "${snow_file}" ]]; then
                    prefix=$(echo "${snow_file}" | cut -d. -f1-3)
                    prefix_new=$(echo "${prefix}" | cut -d. -f2-)
                    for tile in {1..6}; do
                        link_file "${prefix}.sfc_data.tile${tile}.nc" "${prefix_new}.snow_increment.sfc_data.tile${tile}.nc"
                    done
                fi
            done
        fi
    done
    cd "${cwd}"
done
