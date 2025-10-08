#!/bin/bash

#
# ocean_stats.sh - Monitor growing ocean_stats.txt file and create files based on datetime
#
# This script monitors a growing ocean_stats.txt file and creates new files
# based on the datetime values in the second column (format: YYYYMMDDTHHmmss)
#
# Usage: ocean_stats.sh [ocean_stats_file] [output_directory]
#

set -euo pipefail

# Default values
OCEAN_STATS_FILE="${1:-ocean_stats.txt}"
OUTPUT_DIR="${2:-.}"

# Function to log messages with timestamp
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1"
}

# Function to validate datetime format
validate_datetime() {
    local datetime="$1"
    if [[ ! "${datetime}" =~ ^[0-9]{8}T[0-9]{6}$ ]]; then
        return 1
    fi
    return 0
}

# Function to check if output file exists for datetime
output_file_exists() {
    local datetime="$1"

    # Extract date and time components
    local date_part="${datetime:0:8}"      # YYYYMMDD
    local time_part="${datetime:9:6}"      # HHmmss

    # Create filename: ocean_stats_YYYYMMDD_HHmmss.txt
    local output_file="${OUTPUT_DIR}/ocean_stats_${date_part}_${time_part}.txt"

    [[ -f "${output_file}" ]]
}

# Function to create output file based on datetime
create_datetime_file() {
    local datetime="$1"
    local line_content="$2"

    # Extract date and time components
    local date_part="${datetime:0:8}"      # YYYYMMDD
    local time_part="${datetime:9:6}"      # HHmmss

    # Create filename: ocean_stats_YYYYMMDD_HHmmss.txt
    local output_file="${OUTPUT_DIR}/ocean_stats_${date_part}_${time_part}.txt"

    # Only create the file if it doesn't already exist
    if [[ ! -f "${output_file}" ]]; then
        echo "${line_content}" > "${output_file}"
        log_message "Created file: ${output_file}"
    else
        log_message "File already exists: ${output_file}"
    fi
}

# Function to process new lines from ocean_stats.txt
process_new_lines() {
    # Check if ocean_stats file exists
    if [[ ! -f "${OCEAN_STATS_FILE}" ]]; then
        log_message "Ocean stats file ${OCEAN_STATS_FILE} not found"
        return 1
    fi

    log_message "Processing ocean stats file"

    # Process each line in the file
    local line_number=0
    while IFS= read -r line; do
        ((line_number++))

        # Skip empty lines and header lines
        if [[ -z "${line}" ]] || [[ "${line}" =~ ^[[:space:]]*Step, ]]; then
            continue
        fi

        # Parse the line to extract datetime (2nd column)
        # Format: Step, Day, Truncs, Energy/Mass, ...
        # We want the "Day" column which contains datetime
        local datetime
        datetime=$(echo "${line}" | awk -F',' '{gsub(/^[ \t]+|[ \t]+$/, "", $2); print $2}')

        # Validate datetime format
        if validate_datetime "${datetime}"; then
            # Check if output file already exists for this datetime
            if ! output_file_exists "${datetime}"; then
                create_datetime_file "${datetime}" "${line}"
            else
                log_message "Output file already exists for datetime: ${datetime}"
            fi
        else
            log_message "Invalid datetime format: ${datetime} in line ${line_number}: ${line}"
        fi

    done < "${OCEAN_STATS_FILE}"
}

# Function to monitor file continuously
monitor_file() {
    log_message "Starting ocean stats file monitor"
    log_message "Monitoring file: ${OCEAN_STATS_FILE}"
    log_message "Output directory: ${OUTPUT_DIR}"

    # Create output directory if it doesn't exist
    mkdir -p "${OUTPUT_DIR}"

    # Initial processing
    process_new_lines

    # Continuous monitoring
    while true; do
        sleep 5  # Check every 5 seconds
        process_new_lines
    done
}

# Function to process file once and exit
process_once() {
    log_message "Processing ocean stats file once"
    log_message "Input file: ${OCEAN_STATS_FILE}"
    log_message "Output directory: ${OUTPUT_DIR}"

    # Create output directory if it doesn't exist
    mkdir -p "${OUTPUT_DIR}"

    # Process the file
    process_new_lines

    log_message "Processing complete"
}

# Main execution
main() {
    local mode="${3:-once}"

    case "${mode}" in
        "monitor")
            monitor_file
            ;;
        "once")
            process_once
            ;;
        *)
            echo "Usage: $0 [ocean_stats_file] [output_directory] [once|monitor]"
            echo ""
            echo "Arguments:"
            echo "  ocean_stats_file  - Path to ocean_stats.txt file (default: ocean_stats.txt)"
            echo "  output_directory  - Directory for output files (default: current directory)"
            echo "  mode             - Processing mode: 'once' or 'monitor' (default: once)"
            echo ""
            echo "Examples:"
            echo "  $0                                    # Process ocean_stats.txt once"
            echo "  $0 data/ocean_stats.txt ./output     # Process specific file to output dir"
            echo "  $0 ocean_stats.txt ./output monitor  # Continuously monitor file"
            exit 1
            ;;
    esac
}

# Run main function with all arguments
main "$@"
