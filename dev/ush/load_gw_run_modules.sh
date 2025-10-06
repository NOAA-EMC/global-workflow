#! /usr/bin/env bash

###############################################################
# DEPRECATED: This script is deprecated and replaced by load_modules.sh
# This wrapper is maintained for backwards compatibility.
# Please use: source "${HOMEgfs}/dev/ush/load_modules.sh" run
###############################################################

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
source "${script_dir}/load_modules.sh" run
