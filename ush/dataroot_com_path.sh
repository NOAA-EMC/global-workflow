#! /usr/bin/env bash

#---------------------------------------------------------
# dataroot_com_path.sh
#
# This utility is to be used to create a COM structure in the DATAROOT.
# It will replace the root path (up to $COMROOT) with $DATAROOT.
# Use realpath --relative-to to get the relative path from $COMROOT to the target file
# and then prepend $DATAROOT to that path to get the new target path.
#
# Syntax:
#   dataroot_com_path original_com_path
#
#   original_com_path: The original COM path to be transformed.
#
# Example:
#   # Declare COMOUT_ATMOS_ANALYSIS
#   YMD=${PDY} HH=${cyc} declare_from_tmpl -rx \
#       COMOUT_ATMOS_ANALYSIS:COM_ATMOS_ANALYSIS_TMPL
#   # Get the DATAROOT version of the COM path
#   pCOMOUT_ATMOS_ANALYSIS=$(dataroot_com_path "${COMOUT_ATMOS_ANALYSIS}")
#   echo "New COM path in DATAROOT: ${pCOMOUT_ATMOS_ANALYSIS}"
#---------------------------------------------------------

dataroot_com_path() {
    set +x
    if [[ $# -ne 1 ]]; then
        echo "FATAL ERROR in dataroot_com_path: Incorrect number of arguments!"
        echo "Usage: dataroot_com_path original_com_path"
        exit 2
    fi

    local original_com_path=${1}

    if [[ -z "${COMROOT:-}" || -z "${DATAROOT:-}" ]]; then
        echo "FATAL ERROR in dataroot_com_path: COMROOT and DATAROOT must be defined!"
        exit 2
    fi

    local relative_path
    relative_path=$(realpath --relative-to="${COMROOT}" "${original_com_path}")
    local new_com_path="${DATAROOT}/${relative_path}"

    echo "${new_com_path}"
    set -x
}

declare -xf dataroot_com_path
