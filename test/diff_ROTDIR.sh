#! /bin/env bash

#
# Differences relevant output files in two different experiment ROTDIRs.
#   Text files are compared via posix diff. GRiB files are compared via 
#   correlation reported by wgrib2. NetCDF files are compared by using
#   NetCDF operators to calculate a diff then make sure all non-coordinate
#   variable differences are zero. File lists are created by globbing key 
#   directories under the first experiment given.
#
# Syntax:
#     diff_ROTDIR.sh [-c coord_file][-h] noscrub cdate expA expB
#
#       OR
#
#     diff_ROTDIR.sh [-c coord_file][-h] dirA dirB
#
# Arguments:
#     noscrub:        root noscrub where ROTDIRS are held
#     cdate:          experiment date/cycle in YYYYMMDDHH format
#     expA, expB:     experiment ids (PSLOT) to compare
#    
#     dirA, dirB:     full paths to the cycle directories to be compared 
#                       (${noscrub}/${exp}/gfs.${YYYYMMDD}/${cyc})
#
# Options:
#     -c coord_file:  file containing a list of coordinate variables
#     -h:             print usage message and exit
#

set -eu

function usage() {
	#
	# Print usage statement and exit
	#
	echo <<- 'EOF'
		Differences relevant output files in two different experiment ROTDIRs.
		  Text files are compared via posix diff. GRiB files are compared via 
		  correlation reported by wgrib2. NetCDF files are compared by using
		  NetCDF operators to calculate a diff then make sure all non-coordinate
		  variable differences are zero. File lists are created by globbing key 
		  directories under the first experiment given.

		Syntax:
		    diff_ROTDIR.sh [-c coord_file][-h] noscrub cdate expA expB
		
		      OR
		
		    diff_ROTDIR.sh [-c coord_file][-h] dirA dirB
		
		Arguments:
		    noscrub:        root noscrub where ROTDIRS are held
		    cdate:          experiment date/cycle in YYYYMMDDHH format
		    expA, expB:     experiment ids (PSLOT) to compare
		   
		    dirA, dirB:     full paths to the cycle directories to be compared 
		                      (${noscrub}/${exp}/gfs.${YYYYMMDD}/${cyc})
		
		Options:
		    -c coord_file:  file containing a list of coordinate variables
		    -h:             print usage message and exit
	EOF
}


function basename_list() {
	#
	# Expand a list of relative paths by prepending a path
	#
	# Syntax:
	#     basename_list base list_in
	#
	# Arguments:
	#     base:    Common root directory of all paths in list
	#     list_in: List of paths relative to $base/
	#
	# Returns:
	#     List of paths constructed by prepending $base to each 
	#       item in $list_in
	#
	base="${1}"
	list_in="${2}"
	list=""
	for file_in in ${list_in}; do
		list="$list ${base}$(basename $file_in)"
	done
	echo $list
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
	4) # Derive directory paths
		noscrub=$1
		date=$2
		expA=$3
		expB=$4

		YYYYMMDD=$(echo $date | cut -c1-8)
		cyc=$(echo $date | cut -c9-10)
		dirA="$noscrub/$expA/gfs.${YYYYMMDD}/${cyc}"
		dirB="$noscrub/$expB/gfs.${YYYYMMDD}/${cyc}"
		;;
	*) # Unknown option
		echo "${num_args} is not a valid number of arguments, use 2 or 4"
		usage
		exit 1
		;;
esac

temp_file=".diff.nc"

# Contains a bunch of NetCDF Operator shortcuts (will load nco module)
source ./netcdf_op_functions.sh

coord_file="${coord_file:-./coordinates.lst}"

module load wgrib2/2.0.8



## Text files
files=""
files="${files} atmos/input.nml" # This file will be different because of the fix paths
files="${files} $(basename_list 'atmos/' "$dirA/atmos/storms.* $dirA/atmos/trak.*")"
if [[ -d $dirA/ice ]]; then
	files="${files} ice/ice_in"
fi
if [[ -d $dirA/ocean ]]; then
	files="${files} ocean/MOM_input"
fi
# if [[ -d $dirA/wave ]]; then
# 	files="${files} $(basename_list 'wave/station/' "$dirA/wave/station/*bull_tar")"
# fi

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	diff $fileA $fileB || :
done

## GRiB files
files=""
files="${files} $(basename_list 'atmos/' "$dirA/atmos/*grb2* $dirA/atmos/*.flux.*")"
if [[ -d $dirA/wave ]]; then
	files="${files} $(basename_list 'wave/gridded/' "$dirA/wave/gridded/*.grib2")"
fi
if [[ -d $dirA/ocean ]]; then
	files="${files} $(basename_list 'ocean/' "$dirA/ocean/*grb2")"
fi

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	./diff_grib_files.py $fileA $fileB
done

## NetCDF Files
files=""
files="${files} $(basename_list 'atmos/' "$dirA/atmos/*.nc")"
if [[ -d $dirA/ice ]]; then
	files="${files} $(basename_list 'ice/' "$dirA/ice/*.nc")"
fi
if [[ -d $dirA/ocean ]]; then
	files="${files} $(basename_list 'ocean/' "$dirA/ocean/*.nc")"
fi

for file in $files; do
	echo "=== ${file} ==="
	fileA="$dirA/$file"
	fileB="$dirB/$file"
	nccmp -q $fileA $fileB $coord_file
done
