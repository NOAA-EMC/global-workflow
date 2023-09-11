#! /bin/env bash

#
# Differences relevant output files in two UFS model directories. GRiB files 
#   are compared via correlation reported by wgrib2. NetCDF files are compared
#   by using NetCDF operators to calculate a diff then make sure all non-
#   coordinate variable differences are zero.
#
# Syntax:
#     diff_UFS_rundir.sh [-c coord_file][-h] dirA dirB
#
# Arguments:
# 	  dirA, dirB:     full paths to the UFS run directories to be compared
#
# Options:
#     -c coord_file:  file containing a list of coordinate variables
#     -h:             print usage message and exit
#

set -eu

usage() {
	#
	# Print usage statement
	#
	echo <<- 'EOF'
		Differences relevant output files in two UFS model directories. GRiB files 
		  are compared via correlation reported by wgrib2. NetCDF files are compared
		  by using NetCDF operators to calculate a diff then make sure all non-
		  coordinate variable differences are zero.

		Syntax:
		    diff_UFS_rundir.sh [-c coord_file][-h] dirA dirB

		Arguments:
			  dirA, dirB:     full paths to the UFS run directories to be compared

		Options:
		    -c coord_file:  file containing a list of coordinate variables
		    -h:             print usage message and exit
	EOF
}

while getopts ":c:h" option; do
	case "${option}" in
		c) coord_file=${OPTARG} ;;
		h) usage; exit 0 ;;
		*) echo "Unknown option ${option}"; exit 1 ;;
	esac
done

num_args=$#
case $num_args in
	2) # Direct directory paths
		dirA=$1
		dirB=$2
		;;
	*) # Unknown option
		echo "${num_args} is not a valid number of arguments, use 2"
		usage
		exit 1
		;;
esac

source ./netcdf_op_functions.sh
source ./test_utils.sh

temp_file=".diff.nc"
coord_file="${coord_file:-./coordinates.lst}"

# Input files
files="data_table diag_table fd_nems.yaml field_table ice_in input.nml med_modelio.nml \
		model_configure nems.configure pio_in ww3_multi.inp ww3_shel.inp"

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	if [[ -f "$fileA" ]]; then
		diff $fileA $fileB || :
	else
	echo ; echo;
done

# GRiB files
files="$(basename_list '' $dirA/GFSFLX.Grb*)"

module load wgrib2/2.0.8

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	./diff_grib_files.py $fileA $fileB
done

# NetCDF Files
files=""
files="${files} $(basename_list '' $dirA/atmf*.nc $dirA/sfcf*.nc)"
if [[ -d "$dirA/history" ]]; then
	files="$(basename_list 'history/' $dirA/history/*.nc)"
fi

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	nccmp -q $fileA $fileB $coord_file
done

