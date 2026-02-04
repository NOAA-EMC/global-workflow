#!/bin/bash
# convert_to_net.sh
# Script to convert HOMEglobal, PARMglobal, etc. to HOME${NET}, PARM${NET}, etc.
# for NCO operational handoff
#
# Usage: convert_to_net.sh <NET_value> [target_directory]
#
# Example: convert_to_net.sh gfs /path/to/deployment
#
# This script performs selective search/replace of standard EE2 variables:
#   HOMEglobal  -> HOME${NET}
#   PARMglobal  -> PARM${NET}
#   USHglobal   -> USH${NET}
#   SCRglobal   -> SCR${NET}
#   EXECglobal  -> EXEC${NET}
#   FIXglobal   -> FIX${NET}

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

echo "Converting global-workflow standard variables to ${NET} in ${TARGET_DIR}"

# Find all shell scripts, Python files, and configuration files
# Exclude .git, sorc/*, exec/*, lib/*, fix/* to avoid modifying compiled/static content
file_list=$(find "${TARGET_DIR}" \
    -type d \( -name .git -o -name sorc -o -name exec -o -name lib -o -name fix \) -prune -o \
    -type f \( -name "*.sh" -o -name "*.bash" -o -name "*.py" -o -name "*.env" -o -name "*.config" -o -name "*.ecf" -o -name "J*" -o -name "ex*" \) -print)

# Perform the replacements using word boundaries
echo "Performing replacements..."
for file in ${file_list}; do
    if [[ -f "${file}" ]]; then
        sed -i "s/\bHOMEglobal\b/HOME${NET}/g" "${file}"
        sed -i "s/\bPARMglobal\b/PARM${NET}/g" "${file}"
        sed -i "s/\bUSHglobal\b/USH${NET}/g" "${file}"
        sed -i "s/\bSCRglobal\b/SCR${NET}/g" "${file}"
        sed -i "s/\bEXECglobal\b/EXEC${NET}/g" "${file}"
        sed -i "s/\bFIXglobal\b/FIX${NET}/g" "${file}"
    fi
done

echo "Conversion complete!"
echo "Converted global-workflow variables to ${NET}-specific variables"
echo ""
echo "Files processed: $(echo "${file_list}" | wc -w)"
echo ""
echo "IMPORTANT: Before handoff to NCO, verify the changes with:"
echo "  git diff | head -100"
echo "  bash -n <modified_files>"
