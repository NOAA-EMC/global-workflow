#!/bin/bash
# convert_from_net_to_global.sh
# Script to convert HOME${NET}, PARM${NET}, etc. back to HOMEglobal, PARMglobal, etc.
# for development
#
# Usage: convert_from_net_to_global.sh <NET_value> <target_path> [--exclude dir1 dir2 dir3 ...]
# NET_value can be: gfs, gefs, sfs, gcafs, or all (for all NET values)
# target_path can be a file or directory
#
# Example: convert_from_net_to_global.sh gfs /path/to/development --exclude sorc dev parm/archive
# Example: convert_from_net_to_global.sh all /path/to/development
#
# This script performs selective search/replace to revert deployment variables

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
    echo "NET_value can be: gfs, gefs, sfs, gcafs, or all"
    exit 1
fi

NET="$1"
shift

# Define all possible NET values
ALL_NET_VALUES=("gefs" "gfs" "gcafs" "sfs")

# If NET is "all", use all values; otherwise use the single value
if [[ "${NET}" == "all" ]]; then
    NET_LIST=("${ALL_NET_VALUES[@]}")
else
    NET_LIST=("${NET}")
fi

# Initialize target path
TARGET_PATH=""
# Default exclusions: Always exclude the conversion scripts themselves
EXCLUDE_DIRS=("dev/ush/convert_from_net_to_global.sh" "dev/ush/convert_from_global_to_net.sh")

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

# Build display exclude list (filter out conversion scripts)
display_exclude=()
for item in "${EXCLUDE_DIRS[@]}"; do
    if [[ "${item}" != "dev/ush/convert_from_net_to_global.sh" && "${item}" != "dev/ush/convert_from_global_to_net.sh" ]]; then
        display_exclude+=("${item}")
    fi
done

# Display processing header
echo -e "${CYAN}=========================================${NC}"
if [[ "${NET}" == "all" ]]; then
    echo -e "${YELLOW}Processing: Converting NET-specific variables to ${GREEN}global${NC}${YELLOW}-workflow variables from: ${RED}${NET_LIST[*]}${NC}"
else
    echo -e "${YELLOW}Processing: Converting ${RED}${NET}${NC}${YELLOW}-specific variables to ${GREEN}global${NC}${YELLOW}-workflow variables${NC}"
fi
echo -e "${BLUE}Target: ${TARGET_PATH}${NC}"
if [[ ${#display_exclude[@]} -gt 0 ]]; then
    echo -e "${BLUE}Excluding directories: ${display_exclude[*]}${NC}"
fi
echo -e "${CYAN}=========================================${NC}"

# Process each NET value
for current_net in "${NET_LIST[@]}"; do
    echo ""
    echo -e "${YELLOW}Converting for: ${RED}${current_net}${NC} ${YELLOW}→${NC} ${GREEN}global${NC}"

    # List of patterns to convert (reverse of convert_from_global_to_net.sh)
    declare -A patterns=(
        ["HOME${current_net}"]="HOMEglobal"
        ["PARM${current_net}"]="PARMglobal"
        ["USH${current_net}"]="USHglobal"
        ["SCR${current_net}"]="SCRglobal"
        ["EXEC${current_net}"]="EXECglobal"
        ["FIX${current_net}"]="FIXglobal"
    )

    # If target is a single file, process it directly
    if [[ -f "${TARGET_PATH}" ]]; then
        file_modified=false
        for pattern in "${!patterns[@]}"; do
            replacement="${patterns[${pattern}]}"
            # Unconditional replacement - convert all occurrences
            if grep -q "\\b${pattern}\\b" "${TARGET_PATH}" 2> /dev/null; then
                if ! sed -i "s/\\b${pattern}\\b/${replacement}/g" "${TARGET_PATH}"; then
                    echo -e "${RED}ERROR: Failed to process ${TARGET_PATH}${NC}" >&2
                    exit 1
                fi
                file_modified=true
            fi
        done
        if ${file_modified}; then
            echo -e "${GREEN}✓ Processed 1 file for NET=${current_net}${NC}"
        else
            echo -e "${YELLOW}No files to convert for NET=${current_net}${NC}"
        fi
    else
        # Build find command with exclusions for directory
        find_cmd="find \"${TARGET_PATH}\""

        # Build exclusion list for directories
        if [[ ${#EXCLUDE_DIRS[@]} -gt 0 ]]; then
            find_cmd+=" -type d \\("
            first=true
            for exclude_dir in "${EXCLUDE_DIRS[@]}"; do
                # Remove leading ./ if present
                exclude_dir="${exclude_dir#./}"

                # Extract just the directory name (last component of path)
                dir_name=$(basename "${exclude_dir}")

                if ${first}; then
                    find_cmd+=" -name \"${dir_name}\""
                    first=false
                else
                    find_cmd+=" -o -name \"${dir_name}\""
                fi
            done
            find_cmd+=" \\) -prune -o"
        fi

        # Complete find command to get files
        find_cmd+=" -type f -print"

        # Execute find and get file list
        if ! eval "${find_cmd}" > /tmp/convert_files_$$.txt; then
            echo -e "${RED}ERROR: Failed to find files in ${TARGET_PATH}${NC}" >&2
            exit 1
        fi

        # Count files to process
        file_count=$(wc -l < /tmp/convert_files_$$.txt)

        if [[ ${file_count} -eq 0 ]]; then
            echo -e "${YELLOW}No files to convert${NC}"
        else
            echo -e "${BLUE}Processing ${file_count} files...${NC}"

            # Perform the replacements
            failed_files=0
            converted_files=0
            while IFS= read -r file; do
                if [[ -f "${file}" ]]; then
                    # Process file regardless of whether it has global vars
                    # (this allows converting files with mixed NET/global vars)
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

                    if ${file_modified} && ! ${file_failed}; then
                        converted_files=$((converted_files + 1))
                    fi
                fi
            done < /tmp/convert_files_$$.txt

            # Clean up
            rm -f /tmp/convert_files_$$.txt

            if [[ ${converted_files} -eq 0 ]]; then
                echo -e "${YELLOW}No files to convert for NET=${current_net}${NC}"
            elif [[ ${failed_files} -gt 0 ]]; then
                echo -e "${YELLOW}⚠ Converted ${converted_files} files (${failed_files} failed) for NET=${current_net}${NC}"
            else
                echo -e "${GREEN}✓ Converted ${converted_files} files for NET=${current_net}${NC}"
            fi
        fi
    fi
done

echo ""
echo -e "${CYAN}=========================================${NC}"
echo -e "${GREEN}All conversions completed successfully!${NC}"
echo -e "${CYAN}=========================================${NC}"
