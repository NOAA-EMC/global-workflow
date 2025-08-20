#!/usr/bin/env bash

# This script copies the detect_machine.sh script from the global workflow into all of the submodules.
# This ensures that the submodules have the same detect_machine.sh and is only needed for compute builds.

# Locate the head of the global-workflow repository
# Start by getting the full path to this script

# Test if HOMEgfs is defined.  If not, then try to determine it with git rev-parse
_unset_homegfs="NO"
script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
HOMEgfs_=$(cd "${script_dir}" && git rev-parse --show-toplevel)

source_detect_machine="${HOMEgfs_}/ush/detect_machine.sh"

if [[ ! -f "${source_detect_machine}" ]]; then
  echo "FATAL ERROR ${source_detect_machine} does not exist!"
  exit 1
fi

target_files=$(find "${HOMEgfs_}/sorc" -name "detect_machine.sh")
for target_detect_machine in ${target_files}; do
  cp "${source_detect_machine}" "${target_detect_machine}"
done

exit 0
