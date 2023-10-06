#! /usr/bin/env bash

#######
# Script for updating/adding specified netCDF variable metadata
#   attribute values.
#
# Syntax:
#   remap_prep.sh variable_file input_netcdf output_netcdf
#
#   Arguments:
#
#     variable_file: ASCII formatted file containing netCDF variables
#                      and the respective metadata attributes to be
#                      updated/added; the supported format is as
#                      follows.
#
#     <netCDF variable name> <netCDF metadata attribute name>
#       <netCDF metadata values>
#
#       An example using the format described above is as follows.
#
#     SST coordinates geolon,geolat,time
#     uo coordinates geolon_u,geolon_u,time
#     vo coordinates geolon_v,geolon_v,time
#
#       The example above will perform the following task using this
#       script.
#
#       * Assign the netCDF metadata attribute `coordinates` for
#         variable `SST` the values `geolon,geolat,time` and update
#         the `output_netcdf` file path.
#
#       * Assign the netCDF metadata attribute `coordinates` for
#         variable `uo` the values `geolon_u,geolat_u,time` and update
#         the `output_netcdf` file path.
#
#       * Assign the netCDF metadata attribute `coordinates` for
#         variable `vo` the values `geolon_v,geolat_v,time` and update
#         the `output_netcdf` file path.
#
#     input_netcdf: The netCDF-formatted file path containing the
#                     variables defined in `variable_file`.
#
#     output_netcdf: A netCDF-formatted file path to contain the
#                      specified variables remapped to the destination
#                      grid projection.
#######

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/string_utils.sh"

#######

if [[ "$#" -ne 3 ]]; then
    echo "FATAL ERROR: invalid argument syntax provided to ${BASH_SOURCE[0]}"
    echo "Usage: ${BASH_SOURCE[0]} <variable_file> <input_path> <output_path>"
    exit 100
fi

# Collect the command line arguments and check the validity.
variable_file="${1}"
input_path="${2}"
output_path="${3}"

#######

# ncattr_update - Update/add attributes for a variable in a netCDF file.
#
# Description:
#   This function updates the specified attribute for a specified
#     variable in a netCDF file using the `ncatted` command.
#
# Parameters:
#   $1 - The variable name to update.
#   $2 - netCDF variable metadata attribute name.
#   $3 - The coordinates as a comma-separated string.
#
# Global Variables:
#   global_array - An array containing the split coordinates.
#   output_path - The path to the output netCDF file.
#
# Example usage:
#   ncupdate "variable_name" "coords" "lon,lat,time"
#
# This example updates the `coords` attributes for the specified
#   variable and writes the updates to the output netCDF file.

# shellcheck disable=SC2140,SC2124
#   SC2140; disabling due to `ncatted` syntax.
#   SC2124; disabling in order to correctly parse the coordinate
#     arguments.
function ncattr_update(){
    local varname="${1}"
    local ncattr="${2}"
    local coords="${3}"

    comma_split_string "${coords}"
    coords="${global_array[@]}"
    coords_str="$(echo "${coords}" | tr -s ' ')"
    ncattr_str="$(echo "${ncattr}" | tr -s ' ')"
    echo "Adding netCDF attribute ${ncattr_str} values ${coords_str} to variable ${varname} metadata and writing to file ${output_path}"
    (ncatted -O -a "${ncattr_str}","${varname}",c,c," ${coords_str}" "${output_path}" "${output_path}")
}

#######

# Copy the input file path to the output file path.
echo "Copying file ${input_path} to ${output_path} and preparing for variable updates."
cp "${input_path}" "${output_path}"

# Read the configuration file for the the variables to be updated and
# proceed accordingly.
while IFS= read -r line; do

    # Get the attributes for the respective variable.
    varname=$(awk '{print $1}' <<< "${line}")
    ncattr=$(awk '{print $2}' <<< "${line}")
    coords=$(awk '{print $3}' <<< "${line}")
    
    # Update the variable attributes and write the updates to the
    # specified output file (see `output_path`).
    ncattr_update "${varname}" "${ncattr}" "${coords}"
    
done < "${variable_file}"

exit 0
