#!/bin/bash

set -eu

apply_shellcheck() {
  local filename=${1:?}
  local code=${2:-""}
  patch="/tmp/patch.${RANDOM}"  # Do not use PWD, as this will create tons of patch files
  [[ -f "${patch}" ]] && rm -f "${patch}"
  set +e
  shellcheck -i "${code}" "${filename}" -f diff > "${patch}"
  set -e
  patch_size=$(wc -l ${patch} | awk '{print $1}')
  if [[ "${patch_size}" -gt 0 ]]; then
    echo "Apply patch for ${code} on ${filename}"
    git apply "${patch}"
  else
    echo "No patch to apply for ${code} on ${filename}"
  fi
  rm -f "${patch}"
}

file=${1:?}  # File to fix shellcheck errors
apply_shellcheck "${file}" SC2292  # Prefer [[ ]] over [ ] for tests in Bash/Ksh
apply_shellcheck "${file}" SC2250  # Prefer putting braces around variable e.g. ${variable}
apply_shellcheck "${file}" SC2248  # Prefer double quoting
apply_shellcheck "${file}" SC2086  # Double quote to prevent globbing and word splitting
