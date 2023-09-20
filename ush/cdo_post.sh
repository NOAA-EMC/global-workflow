#! /usr/bin/env bash

#######
# Remapping script to remap variables within a netCDF-formatted file
#   to specified destination grid projections.
#
# Syntax:
#   cdo_post.sh variable_file destination_grid output_netcdf
#
#   Arguments:
#
#     variable_file: ASCII formatted file containing attributes
#                      specific to the variables to be remapped to the
#                      destination grid projection(s).
#
#     TODO: Add description of file here.
#
#     dstgrid_config: A netCDF-formatted or CDO-type grid description
#                       file; and example of such a CDO-type grid
#                       description file, to remap a source grid
#                       projected variable to a nominal 1p0 grid
#                       projection is as follows.
#
#     gridtype = lonlat
#     xsize    = 360
#     ysize    = 181
#     xfirst   = -179.0
#     xinc     = 1.0
#     yfirst   = -90.0
#     yinc     = 1.0
#
#       The latter format is preferred as it provides both readability
#       and ease of destination grid configuration.
#
#     output_netcdf: A netCDF-formatted file path to contain the
#                      specified variables remapped to the destination
#                      grid projection.

#######

# Collect the command line arguments and check the validity.
variable_file="${1}"
dstgrid_config="${2}"
output_path="${3}"

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <variable_file> <dstgrid_path> <output_path>"
    exit 1
fi

function _comma_split_string(){
    local string="${1}"

    # Split the comma-delimited string.
    local local_array=()
    IFS="," read -ra items <<< ${string}
    for item in "${items[@]}"; do
	local_array+="${item} "
    done
    global_array=()
    for item in "${local_array[@]}"; do
	IFS=" " read -ra items <<< "${item}"
	for element in "${items[@]}"; do
	    global_array+=("${element} ")
	done
    done
}

function _strip_whitespace(){
    local in_string="${1}"

    # Remove any residual whitespaces.
    out_string=$((echo ${in_string}) | $(command -v sed) "s/ //g")
}
 
function nc_concat(){
    local var_interp_path="${1}"

    tmp_nc_file="${PWD}/tmp_nc.nc"
    if test -e "${output_path}"; then
	echo "netCDF-formatted file path ${output_path} exists; merging ${var_interp_path}"
	$(command -v cdo) merge "${output_path}" "${var_interp_path}" "${tmp_nc_file}"
	$(command -v mv) "${tmp_nc_file}" "${output_path}"
	$(command -v rm) "${var_interp_path}" >> /dev/null
    else
	echo "netCDF-formatted file path ${output_path} does not exist; creating..."
	$(command -v mv) "${var_interp_path}" "${output_path}"
    fi
}

function cdo_remap(){
    local varname="${1}"
    local varfile="${2}"
    local interp_type="${3}"
    local var_interp_path="${PWD}/${varname}.interp.nc"
    
    echo "Remapping variable ${varname} from file ${varfile} using ${interp_type} interpolation."
    $(command -v cdo) "${interp_type}","${dstgrid_config}" -selname,"${varname}" "${varfile}" "${var_interp_path}"
    nc_concat "${var_interp_path}"
}

function cdo_rotate(){
    local varnames="${1}"
    local varfile="${2}"
    local interp_type="${3}"
    local angles="${4}"  
    local var_rotate_path="${PWD}/rotate.nc"    

    # Collect attributes required for the vector rotations.
    _comma_split_string "${varnames}"    
    varname_array=("${global_array[@]}")    
    _comma_split_string "${angles}"
    angle_array=("${global_array[@]}")
    _strip_whitespace "${varname_array[0]}"
    xvar="${out_string}"
    _strip_whitespace "${varname_array[1]}"
    yvar="${out_string}"
    
    # Execute the CDO command based on the grid rotation angle
    # declarations.
    nangles="${#angle_array[@]}"
    echo "Rotating and remapping variables ${xvar} and ${yvar} from file ${xvar_file}."
    if [[ "${nangles}" == 1 ]]; then
	# Define the grid rotation angle variable names.    
	_strip_whitespace "${angle_array[0]}"
	theta="${out_string}"

	# Rotate the specificed vectors accordingly; here it is
	# assumed that a single angle (e.g., `theta`) defines the grid
	# rotation angle; the units of `theta` are assumed to be
	# radians.
	($(command -v cdo) -expr,"xr=${xvar}*cos(${theta})-${yvar}*sin(${theta}); yr=${xvar}*sin(${theta})+${yvar}*cos(${theta})" -selname,"${xvar}","${yvar}","${theta}" ${varfile} "${var_rotate_path}")
	
    elif [[ "${nangles}" == 2 ]]; then
	# Define the grid rotation angle variable names.
	_strip_whitespace "${angle_array[0]}"
	cosang="${out_string}"
	_strip_whitespace "${angle_array[1]}"
	sinang="${out_string}"

	# Rotate the specified vectors accordingly; here it is assumed
	# that the `cos(theta)` and `sin(theta)` have been computed
	# a'priori and assigned the input file variable names defined
	# by `cosang` and `sinang` respectively.
	($(command -v cdo) -expr,"xr=${xvar}*${cosang}-${yvar}*${sinang}; yr=${xvar}*${sinang}+${yvar}*${cosang}" -selname,"${xvar}","${yvar}","${cosang}","${sinang}" ${varfile} "${var_rotate_path}")

	# Remap the respective vector components and rename
	# accordingly.
	cdo_remap "xr" "${var_rotate_path}" "${interp_type}"
	varname_update "xr" "${xvar}" "${output_path}"
	cdo_remap "yr" "${var_rotate_path}" "${interp_type}"
	varname_update "yr" "${yvar}" "${output_path}"
	$(command -v rm) "${var_rotate_path}" >> /dev/null
	
    else
	echo "Vector rotations with ${nangles} attributes is not supported. Aborting!!!"
	exit 2
    fi       	 
}

function varname_update(){
    local old_varname="${1}"
    local new_varname="${2}"
    local ncfile="${3}"

    echo "Renaming variable ${old_varname} to ${new_varname} and writing to file ${ncfile}."
    ($(command -v ncrename) -O -v "${old_varname}","${new_varname}" "${ncfile}")
}

# ----

start_time=$(gdate +%s) # TODO: For debugging locally.
_calling_script=$(basename "${BASH_SOURCE[1]}")
start_time_human=$(gdate -d"@${start_time}" -u) # TODO: For debugging locally.
echo "Begin ${_calling_script} at ${start_time_human}"

# Read the configuration file for the the variables to be remapped and
# proceed accordingly.
while IFS= read -r line; do

    # Get the attributes for the respective variable(s).
    varname=$(echo "${line}" | $(command -v awk) '{print $1}')
    interp_type=$(echo "${line}" | $(command -v awk) '{print $2}')
    srcgrid=$(echo "${line}" | $(command -v awk) '{print $3}')
    rotate=$(echo "${line}" | $(command -v awk) '{print $4}')
    angle=$(echo "${line}" | $(command -v awk) '{print $5}')
    
    if [[ "${rotate}" == 0 ]]; then
    	# No rotation necessary; interpolate/remap the variables and
    	# directly.
	echo "Remapping variable ${varname} without rotation."
	cdo_remap "${varname}" "${srcgrid}" "${interp_type}"
	
    elif [[ "${rotate}" = "1" ]]; then
    	# Rotation necessary; rotate the respective vector quantities
    	# relative to the source grid projection and subsequently
    	# remap the variables to the specified destination grid.
	echo "Remapping variables ${varname} with rotation."
	cdo_rotate "${varname}" "${srcgrid}" "${interp_type}" "${angle}"
	
    else

	echo "fail"

    fi

done < "${variable_file}"

