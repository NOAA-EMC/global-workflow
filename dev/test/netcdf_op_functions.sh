#! /bin/env bash

if [ -t 0 ]; then
	module load nco/4.9.3
fi

## NetCDF operator shortcuts
# From nco.sourceforge.net/nco.html#Filters-for-ncks
# ncattget $att_nm $var_nm $fl_nm : What attributes does variable have?
function ncattget { ncks --trd -M -m ${3} | grep -E -i "^${2} attribute [0-9]+: ${1}" | cut -f 11- -d ' ' | sort ; }
# ncunits $att_val $fl_nm : Which variables have given units?
function ncunits { ncks --trd -m ${2} | grep -E -i " attribute [0-9]+: units.+ ${1}" | cut -f 1 -d ' ' | sort ; }
# ncavg $var_nm $fl_nm : What is mean of variable?
function ncavg {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncwa -y avg -O -C -v ${1} ${2} ${temp_file}
	ncks --trd -H -C -v ${1} ${temp_file} | cut -f 3- -d ' '
	rm ${temp_file}
}
# ncavg $var_nm $fl_nm : What is mean of variable?
function ncavg {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncap2 -O -C -v -s "foo=${1}.avg();print(foo)" ${2} ${temp_file} | cut -f 3- -d ' '
	rm ${temp_file}
}
# ncdmnlst $fl_nm : What dimensions are in file?
function ncdmnlst { ncks --cdl -m ${1} | cut -d ':' -f 1 | cut -d '=' -s -f 1 ; }
# ncvardmnlst $var_nm $fl_nm : What dimensions are in a variable?
function ncvardmnlst { ncks --trd -m -v ${1} ${2} | grep -E -i "^${1} dimension [0-9]+: " | cut -f 4 -d ' ' | sed 's/,//' ; }
# ncvardmnlatlon $var_nm $fl_nm : Does variable contain both lat and lon dimensions?
# function ncvardmnlatlon { flg=$(ncks -C -v ${1} -m ${2} | grep -E -i "${1}\(" | grep -E "lat.*lon|lon.*lat") ; [[ ! -z "$flg" ]] && echo "Yes, ${1} has both lat and lon dimensions" || echo "No, ${1} does not have both lat and lon dimensions" }
# ncdmnsz $dmn_nm $fl_nm : What is dimension size?
function ncdmnsz { ncks --trd -m -M ${2} | grep -E -i ": ${1}, size =" | cut -f 7 -d ' ' | uniq ; }
# ncgrplst $fl_nm : What groups are in file?
function ncgrplst { ncks -m ${1} | grep 'group:' | cut -d ':' -f 2 | cut -d ' ' -f 2 | sort ; }
# ncvarlst $fl_nm : What variables are in file?
function ncvarlst { ncks --trd -m ${1} | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort ; }
# ncmax $var_nm $fl_nm : What is maximum of variable?
function ncmax {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncwa -y max -O -C -v ${1} ${2} ${temp_file}
	ncks --trd -H -C -v ${1} ${temp_file} | cut -f 3- -d ' '
	rm ${temp_file}
}
# ncmax $var_nm $fl_nm : What is maximum of variable?
function ncmax {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncap2 -O -C -v -s "foo=${1}.max();print(foo)" ${2} ${temp_file} | cut -f 3- -d ' '
	rm ${temp_file}
}
# ncmdn $var_nm $fl_nm : What is median of variable?
function ncmdn {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncap2 -O -C -v -s "foo=gsl_stats_median_from_sorted_data(${1}.sort());print(foo)" ${2} ${temp_file} | cut -f 3- -d ' '
	rm ${temp_file}
}
# ncmin $var_nm $fl_nm : What is minimum of variable?
function ncmin {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncap2 -O -C -v -s "foo=${1}.min();print(foo)" ${2} ${temp_file} | cut -f 3- -d ' '
	rm ${temp_file}
}
# ncrng $var_nm $fl_nm : What is range of variable?
function ncrng {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncap2 -O -C -v -s "foo_min=${1}.min();foo_max=${1}.max();print(foo_min,\"%f\");print(\" to \");print(foo_max,\"%f\")" ${2} ${temp_file}
	rm ${temp_file}
}
# ncmode $var_nm $fl_nm : What is mode of variable?
function ncmode {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncap2 -O -C -v -s "foo=gsl_stats_median_from_sorted_data(${1}.sort());print(foo)" ${2} ${temp_file} | cut -f 3- -d ' '
	rm ${temp_file}
}
# ncrecsz $fl_nm : What is record dimension size?
function ncrecsz { ncks --trd -M ${1} | grep -E -i "^Root record dimension 0:" | cut -f 10- -d ' ' ; }
# nctypget $var_nm $fl_nm : What type is variable?
function nctypget { ncks --trd -m -v ${1} ${2} | grep -E -i "^${1}: type" | cut -f 3 -d ' ' | cut -f 1 -d ',' ; }

function nccorr() {
	temp_file=${PTMP:-$HOME}/foo.nc
	ncap2 -O -C -v -s "foo_min=${1}.min();foo_max=${1}.max();print(foo_min,\"%f\");print(\" to \");print(foo_max,\"%f\")" ${2} ${temp_file}
	rm ${temp_file}
}

# Heavily modified from original
function nccmp() {
	# 
	# Compare two netcdf files
	#
	# Uses ncdiff to create a difference of two NetCDFs, then checks to 
	#   make sure all non-coordinate fields of the diff are zero.
	# 
	# Syntax:
	#     nccmp [-q][-z] fileA fileB coord_file
	#
	# Arguments:
	#     fileA, fileB: NetCDFs to be compared
	#     coord_file:   File containing coordinate variables
	#
	# Options:
	#     -q: quiet mode (implies -z)
	#     -z: suppress displaying fields with zero difference
	#
	# Notes:
	#     Will create a temporary file .diff in the $PTMP directory 
	#       if PTMP is defined, otherwise .diff is created in the 
	#       current directory.
	#

	local OPTIND
	suppress_msg=""
	hide_zeros="NO"
	quiet="NO"
	while getopts ":qz" option; do
		case "${option}" in
			q)  quiet="YES" ;&
			z)	suppress_msg=" (Suppressing zero difference fields)"
				hide_zeros="YES"
				;;
			*) echo "Unknown option ${option}"
				;;
		esac
	done
	shift "$((OPTIND-1))"
	fileA="${1}"
	fileB="${2}"
	coord_file="${3:-/dev/null}"
	temp_file="${PTMP:-$(pwd)}/.diff"
	if [[ ${quiet} == "NO" ]]; then
		echo
		echo "Comparing ${fileA} and ${fileB}"
	fi
	# Create diff of the files
	ncdiff ${fileA} ${fileB} ${temp_file} --overwrite
	if [[ ${quiet} == "NO" ]]; then
		echo "Difference report:${suppress_msg}"
		echo "(Coordinate variables will always be non-zero)"
	fi
	count=0
	# Check each variable
	for var in $(ncvarlst ${temp_file}); do
		if [[ $(egrep -o "^${var}\$" ${coord_file} | wc -l) == 0 ]]; then
			# Variable is not in coordinate list
			max=$(ncmax $var $temp_file 2> /dev/null)
			if [[ -z $max ]]; then
				echo "Error reading max of ${var}"
				count=$((count + 1))
				continue
			fi
			min=$(ncmin $var $temp_file 2> /dev/null)
			if [[ -z $min ]]; then
				echo "Error reading min of ${var}"
				count=$((count + 1))
				continue
			fi
			if [[ ${hide_zeros} == "NO" ]] || (( $(echo "$max != 0 || $min != 0" | bc) )); then
				# Min/max is not zero or we are not hiding zeros
				echo "${var}: ${min}..${max}"
				count=$((count + 1))
			fi
		else
			# 
			# ncdiff doesn't difference coordinate variables. Instead coordinates
			#   are just placed in the diff file. While this is generally what we 
			#   want, when checking for equivilence we need to ignore them.
			#
			if [[ ${quiet} == "NO" ]]; then
				echo "$Coordinate ${var} ignored"
			fi
		fi
	done
	rm $temp_file
	echo "${count} differences found"
}


