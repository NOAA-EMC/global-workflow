#! /usr/bin/env bash

#######
# Creates the DATA working directory and cd's into it.
# DATA must be set externally before sourcing this script.
#
# Source this file to execute the setup:
#   source "${HOMEglobal}/dev/ush/setup_data_dir.sh"
#
# Requires in environment:
#   DATA      - path to the working directory
#   err_exit  - (from err_exit.sh)
# Optional:
#   WIPE_DATA - whether to delete any existing DATA [default: "YES"]
#######

if [[ ${WIPE_DATA:-YES} == "YES" ]]; then
    rm -rf "${DATA}"
fi
mkdir -p "${DATA}"
if ! cd "${DATA}"; then
    export err=1
    err_exit "[${BASH_SOURCE[0]}]: ${DATA} does not exist"
fi
