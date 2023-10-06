#! /usr/bin/env bash

#######
# String manipulation functions.
#
# The functions herein are available to all functions sourcing this
#   file; to source this file, do as follows:
#
#   source "${HOMEgfs}/ush/string_utils.sh"
#

#######

# comma_split_string - Split a comma-delimited string into an array.
#
# Description:
#   This function takes a comma-delimited string as input and splits
#   it into an array. Each element in the resulting array is obtained
#   by splitting the input string at commas and then removing leading
#   and trailing spaces.
#
# Parameters:
#   $1 - The comma-delimited string to split.
#
# Global Variables:
#   global_array - An array containing the split elements.
#
# Example usage:
#   comma_split_string "item1,item2 item3,item4"
#   for element in "${global_array[@]}"; do
#       echo "$element"
#   done
#
# This example will split the input string into individual elements
# and print each element on a separate line.
function comma_split_string() {
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

# strip_whitespace - Remove whitespace from a string.
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
#   cleaned_string=$(strip_whitespace "   This is a string   with spaces  ")
#   echo "Cleaned string: \"$cleaned_string\""
#
# This example will remove all leading, trailing, and internal
# whitespace from the input string and display the cleaned result.
function strip_whitespace(){
    local in_string="${1}"

    # shellcheck disable=SC2034
    out_string="${in_string// /}"
}
