#! /usr/bin/env bash

#######
# Defines the setup_data_dir function for use in J-jobs.
#
# Creates a working directory and cd's into it.
#
# Source this file to load the function into the current shell:
#   source "${HOMEglobal}/ush/setup_data_dir.sh"
#
# Usage:
#   setup_data_dir <dir>
#
# Requires in environment:
#   err_exit  - (from err_exit.sh)
# Optional:
#   WIPE_DATA - whether to delete any existing directory [default: "YES"]
#######

setup_data_dir() {
    local dir="${1:?setup_data_dir requires a directory argument}"
    if [[ ${WIPE_DATA:-YES} == "YES" ]]; then
        rm -rf "${dir}"
    fi
    mkdir -p "${dir}"
    if ! cd "${dir}"; then
        export err=1
        err_exit "[${BASH_SOURCE[0]}]: ${dir} does not exist"
    fi
}

declare -xf setup_data_dir
