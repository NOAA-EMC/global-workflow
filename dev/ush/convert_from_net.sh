#!/bin/bash
# convert_from_net.sh
# Script to convert HOME${NET}, PARM${NET}, etc. back to HOMEglobal, PARMglobal, etc.
# for development
#
# Usage: convert_from_net.sh <NET_value> <target_directory>
#
# Example: convert_from_net.sh gfs /path/to/code
#
# This script performs selective search/replace to restore development variables

set -eux

if [[ $# -lt 1 ]]; then
    echo "ERROR: NET value required"
    echo "Usage: $0 <NET_value> [target_directory]"
    exit 1
fi

NET="$1"
TARGET_DIR="${2:-.}"

if [[ ! -d "${TARGET_DIR}" ]]; then
    echo "ERROR: Target directory ${TARGET_DIR} does not exist"
    exit 1
fi

echo "Converting ${NET}-specific variables back to global-workflow standard in ${TARGET_DIR}"

# List of patterns to convert (reverse of convert_to_net.sh)
declare -A patterns=(
    ["HOME${NET}"]="HOMEglobal"
    ["PARM${NET}"]="PARMglobal"
    ["USH${NET}"]="USHglobal"
    ["SCR${NET}"]="SCRglobal"
    ["EXEC${NET}"]="EXECglobal"
    ["FIX${NET}"]="FIXglobal"
)

# Find all relevant files
file_list=$(find "${TARGET_DIR}" \
    -type d \( -name .git -o -name sorc -o -name exec -o -name lib -o -name fix \) -prune -o \
    -type f \( -name "*.sh" -o -name "*.bash" -o -name "*.py" -o -name "*.env" -o -name "*.config" -o -name "*.ecf" -o -name "J*" -o -name "ex*" \) -print)

# Perform the replacements
for file in ${file_list}; do
    if [[ -f "${file}" ]]; then
        for pattern in "${!patterns[@]}"; do
            replacement="${patterns[$pattern]}"
            sed -i "s/\b${pattern}\b/${replacement}/g" "${file}"
        done
        echo "Processed: ${file}"
    fi
done

echo "Conversion complete!"
echo "Restored global-workflow development variables"
