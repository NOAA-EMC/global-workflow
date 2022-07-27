#! /bin/env bash

set -eu

dirA=$1
dirB=$2
temp_file=".diff.nc"

source ./netcdf_op_functions.sh

function basename_list() {
	list=""
	for f in ${1}; do
		list="$list ${2}$(basename $f)"
	done
	echo $list
}

# Input files
files="data_table diag_table fd_nems.yaml field_table ice_in input.nml med_modelio.nml model_configure nems.configure pio_in ww3_multi.inp"
for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	diff $fileA $fileB || :
	echo ; echo;
done

# GRiB files
files="$(basename_list "$dirA/GFSFLX.Grb*")"

module load wgrib2/3.0.2

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	./diff_grib_files.py $fileA $fileB
done

# NetCDF Files
files="$(basename_list "$dirA/history/*.nc" 'history/')"

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	nccmp $fileA $fileB -q
done

