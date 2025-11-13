#!/bin/bash
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

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <target_directory>"
  exit 1
fi

target_dir=$1
cd "${target_dir}" || exit 1

# Check for existence of at least one of gdas.YYYYMMDD, gfs.YYYYMMDD, or enkfgdas.YYYYMMDD directories
dir_list=($(ls -d gdas.* gfs.* enkfgdas.* || true ))

if [ ${#dir_list[@]} -eq 0 ]; then
  echo "No gdas.*, gfs.*, or enkfgdas.* directories found in ${target_dir}."
  exit 1
fi

gdas_list=($(ls -d gdas.* || true ))
gfs_list=($(ls -d gfs.* || true ))
gcdas_list=($(ls -d gcdas.* || true ))
gcafs_list=($(ls -d gcafs.* || true ))
enkfgdas_list=($(ls -d enkfgdas.* || true ))
enkfgfs_list=($(ls -d enkfgfs.* || true ))

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
    *) echo "Unknown directory prefix: ${dir}"; exit 1 ;;
  esac

  cycle_list=($(ls -d ?? || true ))
  for cyc in "${cycle_list[@]}"; do
    if [[ -d "${cwd}/${dir}/${cyc}/analysis/atmos" ]]; then
      cd "${cwd}/${dir}/${cyc}/analysis/atmos"
      for abias_type in abias abias_air abias_int abias_pc; do
        if [[ -f "${system_prefix}.t${cyc}z.${abias_type}" ]]; then
          ln -s "${system_prefix}.t${cyc}z.${abias_type}" "${system_prefix}.t${cyc}z.${abias_type}.txt"
        fi
      done
      if [[ -f "${system_prefix}.t${cyc}z.radstat" ]]; then
        ln -s "${system_prefix}.t${cyc}z.radstat" "${system_prefix}.t${cyc}z.radstat.tar"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.atmi003.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.atmi003.nc" "${system_prefix}.t${cyc}z.increment.atm.i003.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.atminc.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.atminc.nc" "${system_prefix}.t${cyc}z.increment.atm.i006.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.atmi009.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.atmi009.nc" "${system_prefix}.t${cyc}z.increment.atm.i009.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.atma003.ensres.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.atma003.ensres.nc" "${system_prefix}.t${cyc}z.ensres_analysis.atm.a003.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.atmanl.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.atmanl.nc" "${system_prefix}.t${cyc}z.analysis.atm.a006.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.atmanl.ensres.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.atmanl.ensres.nc" "${system_prefix}.t${cyc}z.ensres_analysis.atm.a006.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.atma009.ensres.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.atma009.ensres.nc" "${system_prefix}.t${cyc}z.ensres_analysis.atm.a009.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.cnvstat" ]]; then
        ln -s "${system_prefix}.t${cyc}z.cnvstat" "${system_prefix}.t${cyc}z.cnvstat.tar"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.dtfanl.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.dtfanl.nc" "${system_prefix}.t${cyc}z.analysis.dtf.a006.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.gsistat" ]]; then
        ln -s "${system_prefix}.t${cyc}z.gsistat" "${system_prefix}.t${cyc}z.gsistat.txt"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.oznstat" ]]; then
        ln -s "${system_prefix}.t${cyc}z.oznstat" "${system_prefix}.t${cyc}z.oznstat.tar"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.loginc.txt" ]]; then
        ln -s "${system_prefix}.t${cyc}z.loginc.txt" "${system_prefix}.t${cyc}z.increment.done.txt"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.loganl.txt" ]]; then
        ln -s "${system_prefix}.t${cyc}z.loganl.txt" "${system_prefix}.t${cyc}z.analysis.done.txt"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.sfci003.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfci003.nc" "${system_prefix}.t${cyc}z.increment.sfc.i003.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.sfci006.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfci006.nc" "${system_prefix}.t${cyc}z.increment.sfc.i006.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.sfci009.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfci009.nc" "${system_prefix}.t${cyc}z.increment.sfc.i009.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.sfcanl.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfcanl.nc" "${system_prefix}.t${cyc}z.analysis.sfc.a006.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.sfcinc.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfcinc.nc" "${system_prefix}.t${cyc}z.increment.sfc.i006.nc"
      fi
      if [[ -f "sfc_inc.tile1.nc" ]]; then
        for tile in {1..6}; do
          ln -s "sfc_inc.tile${tile}.nc" "increment.sfc.i006.tile${tile}.nc"
        done
      fi
      if [[ -f "${system_prefix}.t${cyc}z.cubed_sphere_grid_atminc.tile1.nc" ]]; then
        for tile in {1..6}; do
          ln -s "${system_prefix}.t${cyc}z.cubed_sphere_grid_atminc.tile${tile}.nc" "${system_prefix}.t${cyc}z.jedi_increment.atm.i006.tile${tile}.nc"
        done
      fi
    fi
    cd "${cwd}"
    if [[ -d "${cwd}/${dir}/${cyc}/analysis/ocean" ]]; then
      cd "${cwd}/${dir}/${cyc}/analysis/ocean"
      if [[ -f "${system_prefix}.t${cyc}z.ocninc.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.ocninc.nc" "${system_prefix}.t${cyc}z.mom6_increment.i006.nc"
      fi
    fi
    cd "${cwd}"
    if [[ -d "${cwd}/${dir}/${cyc}/analysis/ice" ]]; then
      cd "${cwd}/${dir}/${cyc}/analysis/ice"
      for ice_file in *.cice_model_anl.res.nc; do
        if [[ -f "${ice_file}" ]]; then
          # This gets the first two fields of the filename separated by dots
          prefix=$(echo "${ice_file}" | cut -d. -f1-2)
          ln -s "${ice_file}" "${prefix}.analysis.cice_model.res.nc"
        fi
      done
    fi
    cd "${cwd}"
    if [[ -d "${cwd}/${dir}/${cyc}/analysis/snow" ]]; then
      cd "${cwd}/${dir}/${cyc}/analysis/snow"
      for snow_file in *.sfc_data.tile1.nc; do
        if [[ -f "${snow_file}" ]]; then
          prefix=$(echo "${snow_file}" | cut -d. -f1-3)
          # Keep the date fields of the prefix (the last two fields)
          # Lop off the "snowinc."
          prefix_new=$(echo "${prefix}" | cut -d. -f2-)
          for tile in {1..6}; do
            ln -s "${prefix}.tile${tile}.nc" "${prefix_new}.snow_increment.sfc_data.tile${tile}.nc"
          done
        fi
      done
    fi
  done
  cd "${cwd}"
done

for dir in "${enkfgdas_list[@]}" "${enkfgfs_list[@]}"; do
  cd "${dir}"
  cycle_list=($(ls -d ?? || true ))
  for cyc in "${cycle_list[@]}"; do
    mem_list=($(ls -d mem* || true ))
    for mem in "${mem_list[@]}"; do
      # atmos
      if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/atmos" ]]; then
        cd "${cwd}/${dir}/${cyc}/${mem}/analysis/atmos"
        if [[ -f "${system_prefix}.t${cyc}z.atmi003.nc" ]]; then
          ln -s "${system_prefix}.t${cyc}z.atmi003.nc" "${system_prefix}.t${cyc}z.increment.atm.i003.nc"
        fi
        if [[ -f "${system_prefix}.t${cyc}z.atminc.nc" ]]; then
          ln -s "${system_prefix}.t${cyc}z.atminc.nc" "${system_prefix}.t${cyc}z.increment.atm.i006.nc"
        fi
        if [[ -f "${system_prefix}.t${cyc}z.atmi009.nc" ]]; then
          ln -s "${system_prefix}.t${cyc}z.atmi009.nc" "${system_prefix}.t${cyc}z.increment.atm.i009.nc"
        fi
        # Handle recentered increments
        if [[ -f "${system_prefix}.t${cyc}z.ratmi003.nc" ]]; then
          ln -s "${system_prefix}.t${cyc}z.ratmi003.nc" "${system_prefix}.t${cyc}z.recentered_increment.atm.i003.nc"
        fi
        if [[ -f "${system_prefix}.t${cyc}z.ratminc.nc" ]]; then
          ln -s "${system_prefix}.t${cyc}z.ratminc.nc" "${system_prefix}.t${cyc}z.recentered_increment.atm.i006.nc"
        fi
        if [[ -f "${system_prefix}.t${cyc}z.ratmi009.nc" ]]; then
          ln -s "${system_prefix}.t${cyc}z.ratmi009.nc" "${system_prefix}.t${cyc}z.recentered_increment.atm.i009.nc"
        fi
        # Recentered jedi increments
        if [[ -f "${system_prefix}.t${cyc}z.cubed_sphere_grid_ratminc.tile1.nc" ]]; then
          for tile in {1..6}; do
            ln -s "${system_prefix}.t${cyc}z.cubed_sphere_grid_ratminc.tile${tile}.nc" "${system_prefix}.t${cyc}z.recentered_jedi_increment.atm.i006.tile${tile}.nc"
          done
        fi
        # abias
        for abias_type in abias abias_air abias_int abias_nc; do
          if [[ -f "${system_prefix}.t${cyc}z.${abias_type}.ensmean" ]]; then
            ln -s "${system_prefix}.t${cyc}z.${abias_type}.ensmean" "${system_prefix}.t${cyc}z.${abias_type}.ensmean.txt"
          fi
        done
        # stats
        for stat_type in cnvstat gsistat oznstat radstat; do
          if [[ -f "${system_prefix}.t${cyc}z.${stat_type}.ensmean" ]]; then
            ln -s "${system_prefix}.t${cyc}z.${stat_type}.ensmean" "${system_prefix}.t${cyc}z.${stat_type}.ensmean.tar"
          fi
        done
        if [[ -f "${system_prefix}.t${cyc}z.enkfstat" ]]; then
          ln -s "${system_prefix}.t${cyc}z.enkfstat" "${system_prefix}.t${cyc}z.enkfstat.txt"
        fi
        # surface increments
        for inc_time in 003 006 009; do
          if [[ -f "${system_prefix}.t${cyc}z.sfci${inc_time}.nc" ]]; then
            ln -s "${system_prefix}.t${cyc}z.sfci${inc_time}.nc" "${system_prefix}.t${cyc}z.increment.sfc.i${inc_time}.nc"
          fi
        done
        # sfc_inc tile links
        if [[ -f "sfc_inc.tile1.nc" ]]; then
          for tile in {1..6}; do
            ln -s "sfc_inc.tile${tile}.nc" "increment.sfc.i006.tile${tile}.nc"
          done
        fi
      fi
      # ocean
      if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/ocean" ]]; then
        cd "${cwd}/${dir}/${cyc}/${mem}/analysis/ocean"
        if [[ -f "${system_prefix}.t${cyc}z.ocninc.nc" ]]; then
          ln -s "${system_prefix}.t${cyc}z.ocninc.nc" "${system_prefix}.t${cyc}z.mom6_increment.i006.nc"
        fi
      fi
      # ice
      if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/ice" ]]; then
        cd "${cwd}/${dir}/${cyc}/${mem}/analysis/ice"
        for ice_file in *.cice_model_anl.res.nc; do
          if [[ -f "${ice_file}" ]]; then
            prefix=$(echo "${ice_file}" | cut -d. -f1-2)
            ln -s "${ice_file}" "${prefix}.analysis.cice_model.res.nc"
          fi
        done
      fi
      # snow
      if [[ -d "${cwd}/${dir}/${cyc}/${mem}/analysis/snow" ]]; then
        cd "${cwd}/${dir}/${cyc}/${mem}/analysis/snow"
        for snow_file in *.sfc_data.tile1.nc; do
        if [[ -f "${snow_file}" ]]; then
          prefix=$(echo "${snow_file}" | cut -d. -f1-3)
          prefix_new=$(echo "${prefix}" | cut -d. -f2-)
          for tile in {1..6}; do
            ln -s "${prefix}.tile${tile}.nc" "${prefix_new}.snow_analysis.sfc_data.tile${tile}.nc"
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
        ln -s "${system_prefix}.t${cyc}z.abias.ensmean" "${system_prefix}.t${cyc}z.abias.ensmean.txt"
        ln -s "${system_prefix}.t${cyc}z.abias_air.ensmean" "${system_prefix}.t${cyc}z.abias_air.ensmean.txt"
        ln -s "${system_prefix}.t${cyc}z.abias_int.ensmean" "${system_prefix}.t${cyc}z.abias_int.ensmean.txt"
        ln -s "${system_prefix}.t${cyc}z.abias_pc.ensmean" "${system_prefix}.t${cyc}z.abias_pc.ensmean.txt"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.cnvstat.ensmean" ]]; then
        ln -s "${system_prefix}.t${cyc}z.cnvstat.ensmean" "${system_prefix}.t${cyc}z.cnvstat.ensmean.tar"
        ln -s "${system_prefix}.t${cyc}z.enkfstat" "${system_prefix}.t${cyc}z.enkfstat.txt"
        ln -s "${system_prefix}.t${cyc}z.gsistat.ensmean" "${system_prefix}.t${cyc}z.gsistat.ensmean.tar"
        ln -s "${system_prefix}.t${cyc}z.oznstat.ensmean" "${system_prefix}.t${cyc}z.oznstat.ensmean.tar"
        ln -s "${system_prefix}.t${cyc}z.radstat.ensmean" "${system_prefix}.t${cyc}z.radstat.ensmean.tar"
      fi
      # Surface increments
      if [[ -f "${system_prefix}.t${cyc}z.sfci003.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfci003.nc" "${system_prefix}.t${cyc}z.increment.sfc.i003.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.sfci006.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfci006.nc" "${system_prefix}.t${cyc}z.increment.sfc.i006.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.sfci009.nc" ]]; then
        ln -s "${system_prefix}.t${cyc}z.sfci009.nc" "${system_prefix}.t${cyc}z.increment.sfc.i009.nc"
      fi
      if [[ -f "${system_prefix}.t${cyc}z.loginc.txt" ]]; then
        ln -s "${system_prefix}.t${cyc}z.loginc.txt" "${system_prefix}.t${cyc}z.increment.done.txt"
      fi
    fi
    # snow ensstat
    if [[ -d "${cwd}/${dir}/${cyc}/ensstat/analysis/snow" ]]; then
      cd "${cwd}/${dir}/${cyc}/ensstat/analysis/snow"
      for snow_file in *.sfc_data.tile1.nc; do
        if [[ -f "${snow_file}" ]]; then
          prefix=$(echo "${snow_file}" | cut -d. -f1-3)
          prefix_new=$(echo "${prefix}" | cut -d. -f2-)
          for tile in {1..6}; do
            ln -s "${prefix}.tile${tile}.nc" "${prefix_new}.snow_increment.sfc_data.tile${tile}.nc"
          done
        fi
      done
    fi
  done
  cd "${cwd}"
done
