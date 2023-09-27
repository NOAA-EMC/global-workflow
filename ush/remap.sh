#! /usr/bin/env bash

#######
# Remapping script to remap variables within a netCDF-formatted file
#   to specified destination grid projections.
#
# Syntax:
#   remap.sh variable_file destination_grid output_netcdf
#
#   Arguments:
#
#     variable_file: ASCII formatted file containing attributes
#                      specific to the variables to be remapped to the
#                      destination grid projection(s); supported
#                      formats are as follows.
#
#     <scalar variable> <CDO remapping type> <input variable netCDF file> 0
#     <vector variables> <CDO remapping type> <input variable netCDF file> 1
#        <grid rotation variable(s)>
#
#       An example using the format described above is as follows.
#
#     aice_h remapbil /path/to/ice/model/file 0
#     uo,vo remapbil /path/to/ocean/model/file 1 cos_rot,sin_rot
#
#       The example above will perform the following tasks using this
#       script.
#
#       * Remap the ice model `aice_h` scalar variable, **without**
#         rotation, using the CDO `remapbil` type;
#
#       * Rotate the ocean model current velocity vectors `uo` and
#         `vo` using the `cos_rot` and `sin_rot` values within the
#         ocean model netCDF file and subsequently remap them using
#         the CDO `remapbil` type.
#
#       The tested CDO interpolation types are as follows.
#
#       * remapbil: bilinear interpolation;
#       * remapcon: conservative remapping;
#       * remapnn: nearest-neighbor interpolation.
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

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/string_utils.sh"
REMAP_LOG="${PWD}/remap.log"
rm -f "${REMAP_LOG}" >& /dev/null

# Collect the command line arguments and check the validity.
variable_file="${1}"
dstgrid_config="${2}"
output_path="${3}"

#######

if [[ "$#" -ne 3 ]]; then
    echo "FATAL ERROR: invalid argument syntax provided to ${BASH_SOURCE[0]}"
    echo "Usage: ${BASH_SOURCE[0]} <variable_file> <input_path> <output_path>"
    exit 100
fi

#######

# nc_concat - Concatenate netCDF files and manage file paths.
#
# Description:
#   This function performs the concatenation of two netCDF files:
#   `output_path` and `var_interp_path`. If the `output_path` file
#   exists, the function merges `var_interp_path` into it and manages
#   temporary files accordingly. If `output_path` does not exist, it
#   creates a new netCDF file by renaming `var_interp_path` to
#   `output_path`.
#
# Parameters:
#   $1 - The path to the variable-interpolated netCDF file.
#
# Global Variables:
#   output_path - The path to the output netCDF file.
#
# Example usage:
#   nc_concat "variable_interpolated.nc"
#
# This example will concatenate the variable-interpolated file into
# the output netCDF file, handling the case where the output file may
# or may not exist.
function nc_concat(){
    local var_interp_path="${1}"
    local nc_output_path="${2}"

    tmp_nc_file="${PWD}/tmp_nc.nc"
    if [[ -e "${nc_output_path}" ]]; then
        echo "netCDF-formatted file path ${nc_output_path} exists; merging ${var_interp_path}."
        cdo merge "${nc_output_path}" "${var_interp_path}" "${tmp_nc_file}" >> "${REMAP_LOG}" 2>&1
        mv "${tmp_nc_file}" "${nc_output_path}"
        rm -f "${var_interp_path}" >> /dev/null
    else
        echo "netCDF-formatted file path ${nc_output_path} does not exist and will be created."
        mv "${var_interp_path}" "${nc_output_path}"
    fi
}

#######

# cdo_remap - Perform CDO remapping and concatenate the result.
#
# Description:
#   This function remaps a specific variable `varname` from an input
#   netCDF file `varfile` using the specified interpolation method
#   `interp_type`. It creates an intermediate netCDF file
#   `varname.interp.nc` containing the remapped variable, and then it
#   concatenates this intermediate file into the output netCDF file
#   defined by the global variable `output_path`.
#
# Parameters:
#   $1 - The name of the variable to remap.
#   $2 - The path to the input netCDF file.
#   $3 - The interpolation method to use (e.g., "remapbil").
#
# Global Variables:
#   output_path - The path to the output netCDF file.
#   dstgrid_config - The configuration for destination grid specifications.
#
# Example usage:
#   cdo_remap "temperature" "input_file.nc" "remapbil"
#
# This example remaps the "temperature" variable using bilinear
# interpolation from the input file and appends the result to the
# output netCDF file.
function cdo_remap(){
    local varname="${1}"
    local varfile="${2}"
    local interp_type="${3}"
    local var_interp_path="${PWD}/${varname}.interp.nc"
    
    echo "Remapping variable ${varname} from file ${varfile} using ${interp_type} interpolation."
    cdo "${interp_type}","${dstgrid_config}" -selname,"${varname}" "${varfile}" "${var_interp_path}" >> "${REMAP_LOG}" 2>&1
    nc_concat "${var_interp_path}" "${output_path}"
}

#######

# cdo_rotate - Rotate and remap vector variables in a netCDF file.
#
# Description:
#   This function rotates vector variables specified by `varnames` in
#   an input netCDF file `varfile` using the specified interpolation
#   method `interp_type` and the rotation angles defined in
#   `angles`. It creates an intermediate netCDF file `rotate.nc` to
#   store the rotated variables, performs remapping if required, and
#   renames the components before appending them to the output netCDF
#   file defined by the global variable `output_path`.
#
# Parameters:
#   $1 - Comma-delimited variable names to rotate (e.g., "u,v").
#   $2 - The path to the input netCDF file.
#   $3 - The interpolation method to use (e.g., "remapbil").
#   $4 - Comma-delimited rotation angles or trigonometric functions
#        (e.g., "theta" or "cosang,sinang").
#
# Global Variables:
#   output_path - The path to the output netCDF file.
#
# Example usage:
#   cdo_rotate "u,v" "input_file.nc" "remapbil" "theta"
#
# This example rotates the "u" and "v" vector components using a
# single angle variable "theta" and appends the rotated vectors to the
# output netCDF file.
#
# Vector Rotation Options:
#   nangles = 1:
#      Rotate the specificed vectors accordingly; here it is assumed
#      that a single angle (e.g., `theta`) defines the grid rotation
#      angle; the units of `theta` are assumed to be radians.
#
#   nangles = 2:
#     Rotate the specified vectors accordingly; here it is assumed
#     that the `cos(theta)` and `sin(theta)` have been computed
#     a'priori and assigned the input file variable names defined by
#     `cosang` and `sinang` respectively.
#
function cdo_rotate(){    
    local varnames="${1}"
    local varfile="${2}"
    local interp_type="${3}"
    local angles="${4}"  
    local var_rotate_path="${PWD}/rotate.nc"
    
    comma_split_string "${varnames}"    
    varname_array=("${global_array[@]}")    
    comma_split_string "${angles}"
    angle_array=("${global_array[@]}")
    strip_whitespace "${varname_array[0]}"
    xvar="${out_string}"
    strip_whitespace "${varname_array[1]}"
    yvar="${out_string}"
    nangles="${#angle_array[@]}"
    
    echo "Rotating and remapping variables ${xvar} and ${yvar} from file ${varfile}."
    if (( nangles == 1 )); then
        strip_whitespace "${angle_array[0]}"
        theta="${out_string}"
        cdo -expr,"xr=cos(${theta})*${xvar}+sin(${theta})*${yvar}; yr=cos(${theta})*${yvar}-sin(${theta})*${xvar}" -selname,"${xvar}","${yvar}","${theta}" "${varfile}" "${var_rotate_path}" >> "${REMAP_LOG}" 2>&1
    elif (( nangles == 2 )); then
        strip_whitespace "${angle_array[0]}"
        cosang="${out_string}"
        strip_whitespace "${angle_array[1]}"
        sinang="${out_string}"
        cdo -expr,"xr=${cosang}*${xvar}+${sinang}*${yvar}; yr=${cosang}*${yvar}-${sinang}*${xvar}" -selname,"${xvar}","${yvar}","${cosang}","${sinang}" "${varfile}" "${var_rotate_path}" >> "${REMAP_LOG}" 2>&1
    else
        echo "FATAL ERROR: Vector rotations with ${nangles} attributes is not supported. Aborting!!!"
        exit 102
    fi
    
    cdo_remap "xr" "${var_rotate_path}" "${interp_type}"
    varname_update "xr" "${xvar}" "${output_path}"
    cdo_remap "yr" "${var_rotate_path}" "${interp_type}"
    varname_update "yr" "${yvar}" "${output_path}"
    rm -f "${var_rotate_path}" >> /dev/null
}

#######

# varname_update - Rename a variable in a netCDF file and write to a new file.
#
# Description: This function renames a variable in a netCDF file from
#   `old_varname` to `new_varname` and writes the result to the
#   (existing) netCDF file specified by `ncfile`.
#
# Parameters:
#   $1 - The old variable name to be renamed.
#   $2 - The new variable name to use.
#   $3 - The path to the input netCDF file; note that this is updated
#        with every call to this function.
#
# Example usage:
#   varname_update "old_variable" "new_variable" "input_file.nc"
#
# This example renames the variable "old_variable" to "new_variable"
# in the input netCDF file and writes the result to the specified
# netCDF file.
function varname_update(){
    local old_varname="${1}"
    local new_varname="${2}"
    local ncfile="${3}"

    echo "Renaming variable ${old_varname} to ${new_varname} and writing to file ${ncfile}."
    ncrename -O -v "${old_varname}","${new_varname}" "${ncfile}" >> "${REMAP_LOG}" 2>&1
}

#######

# Clobber the output file; otherwise the respective variables will be
# written multiple times to the file; for example VAR, VAR_1, VAR_2,
# etc.,
rm -f "${output_path}" >& /dev/null

# Read the configuration file for the the variables to be remapped and
# proceed accordingly.
while IFS= read -r line; do

    # Get the attributes for the respective variable(s).
    varname=$(awk '{print $1}' <<< "${line}")
    interp_type=$(awk '{print $2}' <<< "${line}")
    srcgrid=$(awk '{print $3}' <<< "${line}")
    rotate=$(awk '{print $4}' <<< "${line}")
    angle=$(awk '{print $5}' <<< "${line}")
    
    if (( rotate == 0 )); then
        # No rotation necessary; interpolate/remap the variables and
        # directly.
        echo "Remapping variable ${varname} without rotation."
        cdo_remap "${varname}" "${srcgrid}" "${interp_type}"
	
    elif (( rotate == 1 )); then
        # Rotation necessary; rotate the respective vector quantities
        # relative to the source grid projection and subsequently
        # remap the variables to the specified destination grid.
        echo "Remapping variables ${varname} with rotation."
        cdo_rotate "${varname}" "${srcgrid}" "${interp_type}" "${angle}"
	
    else
        echo "FATAL ERROR: Rotation option ${rotate} not recognized. Aborting!!!"
        exit 101
    fi

done < "${variable_file}"

exit 0
