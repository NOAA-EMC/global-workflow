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

# Collect the command line arguments and check the validity.
variable_file="${1}"
input_path="${2}"
output_path="${3}"

#######

if [[ "$#" -ne 3 ]]; then
    echo "FATAL ERROR: invalid argument syntax provided to ${BASH_SOURCE[0]}"
    echo "Usage: ${BASH_SOURCE[0]} <variable_file> <input_path> <output_path>"
    exit 100
fi

#######

# _comma_split_string - Split a comma-delimited string into an array.
#
# Description:
#   This function takes a comma-delimited string as input and splits
#     it into an array. Each element in the resulting array is
#     obtained by splitting the input string at commas and then
#     removing leading and trailing spaces.
#
# Parameters:
#   $1 - The comma-delimited string to split.
#
# Global Variables:
#   global_array - An array containing the split elements.
#
# Example usage:
#   _comma_split_string "item1,item2,item3,item4"
#   for element in "${global_array[@]}"; do
#       echo "$element"
#   done
#
# This example will split the input string into individual elements
#   and print each element on a separate line.
function _comma_split_string() {
    local string="${1}"

    local local_array=()
    global_array=()
    IFS="," read -ra items <<< "${string}"
    for item in "${items[@]}"; do
        local_array+=("${item} ")
    done
    for item in "${local_array[@]}"; do
        IFS=" " read -ra items <<< "${item}"
        for element in "${items[@]}"; do
            global_array+=("${element} ")
        done
    done
}

#######

# _strip_whitespace - Remove whitespace from a string.
#
# Description:
#   This function takes an input string and removes all whitespace
#   characters (spaces, tabs, and newline characters) to produce a
#   cleaned output string.
#
# Parameters:
#   $1 - The input string from which whitespace will be removed.
#
# Return:
#   The cleaned string with no whitespace.
#
# Example usage:
#   cleaned_string=$(_strip_whitespace "   This is a string   with spaces  ")
#   echo "Cleaned string: \"$cleaned_string\""
#
# This example will remove all leading, trailing, and internal
# whitespace from the input string and display the cleaned result.
function _strip_whitespace(){
    local in_string="${1}"

    out_string=$(echo "${in_string}" | sed "s/ //g")
}

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
function ncattr_update(){
    local varname="${1}"
    local ncattr="${2}"
    local coords="${3}"

    _comma_split_string "${coords}"
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
