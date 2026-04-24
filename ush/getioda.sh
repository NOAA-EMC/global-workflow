#! /usr/bin/env bash

#===============================================================================
#
#   FILE: get_ioda.sh
#
#   DESCRIPTION: This script transfers multi-component observational data dumps
#                (e.g., IODA files) from a source staging directory to the target
#                model runtime directory. It dynamically loops through all
#                available component subdirectories (e.g., atmos, ocean, land),
#                verifies the presence of a completion status log (*status.log)
#                for each component, and copies the relevant cycle-prefixed files.
#
#
#    ARGUMENTS:
#       $1 - YMD        : Date of the cycle in YYYYMMDD format.
#       $2 - HH         : Hour of the cycle (e.g., 00, 06, 12, 18).
#       $3 - RUN        : Model run identifier (e.g., gfs, gdas).
#       $4 - SOURCE_DIR : (Optional) Override for the source IODA dump directory.
#       $5 - TARGET_DIR : (Optional) Override for the target runtime directory.

YMD=${1:-""}
HH=${2:-""}
RUN=${3:-""}
SOURCE_DIR=${4:-${IODADIR}/${RUN}${DUMP_SUFFIX}.${YMD}/${HH}}
TARGET_DIR=${5:-${ROTDIR}/${RUN}.${YMD}/${HH}}

DUMP_SUFFIX=${DUMP_SUFFIX:-""}

# Exit if SOURCE_DIR does not exist
if [[ ! -s "${SOURCE_DIR}" ]]; then
    echo "FATAL ERROR: DUMP SOURCE_DIR=${SOURCE_DIR} does not exist"
    exit 99
fi

# Create TARGET_DIR if is does not exist
if [[ ! -s "${TARGET_DIR}" ]]; then
    mkdir -p "${TARGET_DIR}"
fi

# Set file prefix
prefix="${RUN}.t${HH}z."

# loop through top level component directories (e.g. atmos, ocean, land, ice)
for compdir in "${SOURCE_DIR}"/*/; do
    compdir=${compdir%*/}
    compdir=${compdir##*/}
    # Skip if not a directory
    if [[ ! -d "${SOURCE_DIR}/${compdir}/" ]]; then
        continue
    fi
    # check if status file does not exist
    # Check if any status log files exist using array expansion
    set +f
    status_files=("${SOURCE_DIR}/${compdir}/${prefix}"*status.log)
    if [[ ! -e "${status_files[0]}" ]]; then
        echo "FATAL ERROR: completion log file NOT FOUND in ${SOURCE_DIR}/${compdir}"
        exit 99
    fi
    echo "Processing component directory: ${compdir}"
    set -f
    # Create component directory in TARGET_DIR if it does not exist
    if [[ ! -s "${TARGET_DIR}/${compdir}" ]]; then
        mkdir -p "${TARGET_DIR}/${compdir}"
    fi
    # Link files from SOURCE_DIR to TARGET_DIR
    if [[ ! -d "${SOURCE_DIR}/${compdir}" ]]; then
        echo "FATAL ERROR: '${SOURCE_DIR}/${compdir}' does not exist, ABORT!"
        exit 99
    fi
    # Use shell globbing instead of iterating over ls output. Enable nullglob so the loop
    # simply skips when no matches are found.
    set +f
    shopt -s nullglob
    for source_file in "${SOURCE_DIR}/${compdir}/${prefix}"*; do
        if [[ ! -e "${source_file}" ]]; then
            continue
        fi
        targ_file=$(basename "${source_file}")
        cpreq "${source_file}" "${TARGET_DIR}/${compdir}/${targ_file}"
    done
    shopt -u nullglob
    set -f
done

exit 0
