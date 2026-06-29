#! /usr/bin/env bash

#===============================================================================
#
#   FILE: getdump.sh
#
#   DESCRIPTION: This script copies observational data dump files from a source
#                dump directory to a target runtime directory for a specific
#                model run, cycle time, and component. It strictly requires the
#                presence of the status file (updated.status.tm00.bufr_d) to
#                ensure the dump is complete before initiating the copy process.
#    ARGUMENTS:
#       $1 - YMD        : Date of the cycle in YYYYMMDD format.
#       $2 - HH         : Hour of the cycle (e.g., 00, 06, 12, 18).
#       $3 - RUN        : Model run identifier (e.g., gfs, gdas).
#       $4 - SOURCE_DIR : (Optional) Override for the source dump directory.
#       $5 - TARGET_DIR : (Optional) Override for the target runtime directory.

COMPONENT=${COMPONENT:-atmos}

YMD=${1:-""}
HH=${2:-""}
RUN=${3:-""}
SOURCE_DIR=${4:-${DMPDIR}/${RUN}${DUMP_SUFFIX}.${YMD}/${HH}/${COMPONENT}}
TARGET_DIR=${5:-${ROTDIR}/${RUN}.${YMD}/${HH}/${COMPONENT}}

DUMP_SUFFIX=${DUMP_SUFFIX:-""}

# Exit if SOURCE_DIR does not exist
if [[ ! -d "${SOURCE_DIR}" ]]; then
    echo "***ERROR*** DUMP SOURCE_DIR=${SOURCE_DIR} does not exist"
    exit 99
fi

# Create TARGET_DIR if is does not exist
if [[ ! -s "${TARGET_DIR}" ]]; then
    mkdir -p "${TARGET_DIR}"
fi

# Set file prefix
prefix="${RUN}.t${HH}z."

# Link dump files from SOURCE_DIR to TARGET_DIR
cd "${SOURCE_DIR}" || exit 1
if [[ -s "${prefix}updated.status.tm00.bufr_d" ]]; then
    for file in "${prefix}"*; do
        cpreq "${SOURCE_DIR}/${file}" "${TARGET_DIR}/${file}"
    done
else
    echo "***ERROR*** ${prefix}updated.status.tm00.bufr_d NOT FOUND in ${SOURCE_DIR}"
    exit 99
fi

exit 0
