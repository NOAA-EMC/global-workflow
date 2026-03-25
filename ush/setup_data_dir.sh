#! /usr/bin/env bash

#######
# Creates a working directory and cd's into it.
#
# Source this file with a directory argument:
#   source "${HOMEglobal}/ush/setup_data_dir.sh" <dir>
#
# Requires in environment:
#   err_exit  - (from err_exit.sh)
# Optional:
#   WIPE_DATA - whether to delete any existing directory [default: "YES"]
#######

local dir="${1:?setup_data_dir.sh requires a directory argument}"
if [[ ${WIPE_DATA:-YES} == "YES" ]]; then
    rm -rf "${dir}"
fi
mkdir -p "${dir}"
if ! cd "${dir}"; then
    export err=1
    err_exit "[${BASH_SOURCE[0]}]: ${dir} does not exist"
fi
