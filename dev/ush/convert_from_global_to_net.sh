#!/bin/bash
# convert_from_global_to_net.sh
# Script to convert HOMEglobal, PARMglobal, etc. to HOME${NET}, PARM${NET}, etc.
# for operational deployment
#
# Usage: convert_from_global_to_net.sh <NET_value> <target_path> [--exclude dir1 dir2 dir3 ...]
#
# NET_value can be: gfs, gefs, sfs, or gcafs (NOT 'all' - use specific NET)
# target_path can be a file or directory
#
# Example: convert_from_global_to_net.sh gfs /path/to/deployment --exclude sorc dev parm/archive
#
# This script performs selective search/replace for deployment variables

set -eu

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Cleanup function for temporary files
cleanup() {
    local exit_code=$?
    if [[ -f "/tmp/convert_files_$$.txt" ]]; then
        rm -f "/tmp/convert_files_$$.txt"
    fi
    if [[ ${exit_code} -ne 0 ]]; then
        echo -e "${RED}Error: Script failed with exit code ${exit_code}${NC}" >&2
    fi
}

# Set trap to ensure cleanup on exit
trap cleanup EXIT ERR

# Parse arguments
if [[ $# -lt 2 ]]; then
    echo "ERROR: NET value and target path required"
    echo "Usage: $0 <NET_value> <target_path> [--exclude dir1 dir2 dir3 ...]"
    echo "NET_value must be one of: gfs, gefs, sfs, gcafs"
    exit 1
fi

NET="$1"
shift

# Define all possible NET values for validation
ALL_NET_VALUES=("gefs" "gfs" "gcafs" "sfs")

# Validate NET value - must be specific, NOT 'all'
if [[ "${NET}" == "all" ]]; then
    echo -e "${RED}ERROR: 'all' is not supported for convert_from_global_to_net.sh${NC}" >&2
    echo -e "${YELLOW}You must specify a single NET value: gfs, gefs, sfs, or gcafs${NC}" >&2
    echo -e "${YELLOW}Use convert_from_net_to_global.sh with 'all' to revert changes${NC}" >&2
    exit 1
fi

# Validate NET is one of the allowed values
valid_net=false
for net_val in "${ALL_NET_VALUES[@]}"; do
    if [[ "${NET}" == "${net_val}" ]]; then
        valid_net=true
        break
    fi
done

if ! ${valid_net}; then
    echo -e "${RED}ERROR: Invalid NET value '${NET}'${NC}" >&2
    echo -e "${YELLOW}Must be one of: ${ALL_NET_VALUES[*]}${NC}" >&2
    exit 1
fi

# Initialize target path
TARGET_PATH=""
EXCLUDE_DIRS=()

# Parse remaining arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --exclude)
            shift
            # Collect all remaining arguments as exclude directories
            while [[ $# -gt 0 ]]; do
                EXCLUDE_DIRS+=("$1")
                shift
            done
            ;;
        *)
            if [[ -z "${TARGET_PATH}" ]]; then
                TARGET_PATH="$1"
            else
                echo "ERROR: Unexpected argument: $1"
                exit 1
            fi
            shift
            ;;
    esac
done

# Check if target path exists
if [[ ! -e "${TARGET_PATH}" ]]; then
    echo -e "${RED}ERROR: Target path ${TARGET_PATH} does not exist${NC}" >&2
    exit 1
fi

# List of directories and files to exclude from processing
exclude_items=(
    "sorc"
    "dev/ush/convert_from_net_to_global.sh"
    "dev/ush/convert_from_global_to_net.sh"
)

# Build grep exclusion pattern (includes all items)
exclude_pattern=""
for item in "${exclude_items[@]}"; do
    if [[ -n "${exclude_pattern}" ]]; then
        exclude_pattern="${exclude_pattern}|"
    fi
    exclude_pattern="${exclude_pattern}${item}"
done

# Display what we're excluding (filter out conversion scripts from display)
display_exclude=()
for item in "${exclude_items[@]}"; do
    if [[ "${item}" != "dev/ush/convert_from_net_to_global.sh" && "${item}" != "dev/ush/convert_from_global_to_net.sh" ]]; then
        display_exclude+=("${item}")
    fi
done

if [[ ${#display_exclude[@]} -gt 0 ]]; then
    echo "Excluding directories: ${display_exclude[*]}"
fi

# Display processing header
echo -e "${CYAN}=========================================${NC}"
echo -e "${YELLOW}Processing: Converting ${RED}global${NC}${YELLOW}-workflow variables to ${GREEN}${NET}${NC}${YELLOW}-specific variables${NC}"
echo -e "${BLUE}Target: ${TARGET_PATH}${NC}"
if [[ ${#EXCLUDE_DIRS[@]} -gt 0 ]]; then
    echo -e "${BLUE}Excluding directories: ${EXCLUDE_DIRS[*]}${NC}"
fi
echo -e "${CYAN}=========================================${NC}"

echo ""
echo -e "${YELLOW}Converting for: ${RED}global${NC} ${YELLOW}→${NC} ${GREEN}${NET}${NC}"

# List of patterns to convert
declare -A patterns=(
    ["HOMEglobal"]="HOME${NET}"
    ["PARMglobal"]="PARM${NET}"
    ["USHglobal"]="USH${NET}"
    ["SCRglobal"]="SCR${NET}"
    ["EXECglobal"]="EXEC${NET}"
    ["FIXglobal"]="FIX${NET}"
)

# If target is a single file, process it directly
if [[ -f "${TARGET_PATH}" ]]; then
    # Pre-check: Skip if ANY NET-specific variable already exists
    if grep -qE '\b(HOME|PARM|USH|SCR|EXEC|FIX)(gfs|gefs|sfs|gcafs)\b' "${TARGET_PATH}" 2> /dev/null; then
        echo -e "${YELLOW}⚠ File already has NET-specific variables - skipped${NC}"
        exit 0
    fi

    file_modified=false
    for pattern in "${!patterns[@]}"; do
        replacement="${patterns[${pattern}]}"
        if grep -q "\\b${pattern}\\b" "${TARGET_PATH}" 2> /dev/null; then
            if ! sed -i "s/\\b${pattern}\\b/${replacement}/g" "${TARGET_PATH}"; then
                echo -e "${RED}ERROR: Failed to process ${TARGET_PATH}${NC}" >&2
                exit 1
            fi
            file_modified=true
        fi
    done

    if ${file_modified}; then
        echo -e "${GREEN}✓ Processed 1 file for NET=${NET}${NC}"
    else
        echo -e "${YELLOW}No files to convert for NET=${current_net}${NC}"
    fi
else
    # Build find command with exclusions for directory
    # Build find command with excluded directories (properly handle subdirectories)
    if [[ ${#EXCLUDE_DIRS[@]} -gt 0 ]]; then
        exclude_args=""
        for exclude_dir in "${EXCLUDE_DIRS[@]}"; do
            exclude_args+="-name \"$(basename "${exclude_dir}")\" -o "
        done
        exclude_args="${exclude_args% -o }"
        eval "find \"${TARGET_PATH}\" -type d \( ${exclude_args} \) -prune -o -type f -print" > /tmp/convert_files_$$.txt
    else
        find "${TARGET_PATH}" -type f > /tmp/convert_files_$$.txt
    fi

    file_count=$(wc -l < /tmp/convert_files_$$.txt)
    if [[ ${file_count} -eq 0 ]]; then
        echo -e "${YELLOW}No files to convert for NET=${current_net}${NC}"
        rm -f /tmp/convert_files_$$.txt
        continue
    fi

    # Count files to process
    echo -e "${BLUE}Processing ${file_count} files...${NC}"

    # Perform the replacements
    failed_files=0
    skipped_files=0
    while IFS= read -r file; do
        if [[ -f "${file}" ]]; then
            # Pre-check: Skip file if it contains ANY NET-specific variable (gfs, gefs, sfs, gcafs)
            if grep -qE '\b(HOME|PARM|USH|SCR|EXEC|FIX)(gfs|gefs|sfs|gcafs)\b' "${file}" 2> /dev/null; then
                skipped_files=$((skipped_files + 1))
                continue
            fi

            # Proceed with conversion only if no NET-specific vars found
            file_modified=false
            file_failed=false
            for pattern in "${!patterns[@]}"; do
                replacement="${patterns[${pattern}]}"
                if grep -q "\\b${pattern}\\b" "${file}" 2> /dev/null; then
                    if ! sed -i "s/\\b${pattern}\\b/${replacement}/g" "${file}"; then
                        echo -e "${RED}ERROR: sed failed on ${file}${NC}" >&2
                        failed_files=$((failed_files + 1))
                        file_failed=true
                        break
                    fi
                    file_modified=true
                fi
            done

            if ! ${file_modified} && ! ${file_failed}; then
                skipped_files=$((skipped_files + 1))
            fi
        fi
    done < /tmp/convert_files_$$.txt

    # Clean up
    rm -f /tmp/convert_files_$$.txt

    files_converted=$((file_count - failed_files - skipped_files))
    if [[ ${files_converted} -eq 0 ]]; then
        echo -e "${YELLOW}No files to convert for NET=${NET}${NC}"
    elif [[ ${failed_files} -gt 0 ]]; then
        echo -e "${YELLOW}⚠ Converted ${files_converted}/${file_count} files (${failed_files} failed) for NET=${NET}${NC}"
    else
        echo -e "${GREEN}✓ Converted ${files_converted}/${file_count} files for NET=${NET}${NC}"
    fi
fi
echo -e "${GREEN}Completed!${NC}"

echo ""
echo -e "${CYAN}=========================================${NC}"
echo -e "${GREEN}Conversion to NET=${NET} completed successfully!${NC}"
