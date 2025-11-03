#! /usr/bin/env bash

YMD=${1:-""}
HH=${2:-""}
RUN=${3:-""}
SOURCE_DIR=${4:-${IODADIR}/${RUN}${DUMP_SUFFIX}.${YMD}/${HH}}
TARGET_DIR=${5:-${ROTDIR}/${RUN}.${YMD}/${HH}}

DUMP_SUFFIX=${DUMP_SUFFIX:-""}

# Exit if SOURCE_DIR does not exist
if [[ ! -s "${SOURCE_DIR}" ]]; then
   echo "***ERROR*** DUMP SOURCE_DIR=${SOURCE_DIR} does not exist"
   exit 99
fi
   
# Create TARGET_DIR if is does not exist
if [[ ! -s "${TARGET_DIR}" ]]; then
   mkdir -p "${TARGET_DIR}"
fi

# Set file prefix
prefix="${RUN}.t${HH}z."

# loop through top level component directories (e.g. atmos, ocean, land, ice)
for compdir in "${SOURCE_DIR}"/*/ ; do
    compdir=${compdir%*/}
    compdir=${compdir##*/}
    # Skip if not a directory
    if [[ ! -d "${SOURCE_DIR}/${compdir}/" ]]; then
        continue
    fi
    echo "Processing component directory: ${compdir}"
    # Create component directory in TARGET_DIR if it does not exist
    if [[ ! -s "${TARGET_DIR}/${compdir}" ]]; then
       mkdir -p "${TARGET_DIR}/${compdir}"
    fi
    # loop through secondary level directories (e.g. sfc, atm, sss, etc)
    for subdir in "${SOURCE_DIR}/${compdir}"/*/ ; do
        subdir=${subdir%*/}
        subdir=${subdir##*/}
        # Skip if not a directory
        if [[ ! -d "${SOURCE_DIR}/${compdir}/${subdir}" ]]; then
            continue
        fi
        echo "  Processing subdirectory: ${subdir}"
        # Create subdirectory in TARGET_DIR if it does not exist
        if [[ ! -s "${TARGET_DIR}/${compdir}/${subdir}" ]]; then
           mkdir -p "${TARGET_DIR}/${compdir}/${subdir}"
        fi
        # Link files from SOURCE_DIR to TARGET_DIR
        cd "${SOURCE_DIR}/${compdir}/${subdir}"
        if [[ -s "${prefix}?status.log" ]]; then
            for file in $(ls ${prefix}*); do
                ${NLN} "${SOURCE_DIR}/${compdir}/${subdir}/${file}" "${TARGET_DIR}/${compdir}/${subdir}/${file}"
            done
        else
            echo "***ERROR*** completion log file NOT FOUND in ${SOURCE_DIR}/${compdir}/${subdir}"
            exit 99
        fi
    done
done

exit 0
