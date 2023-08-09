#! /bin/bash

set -eux

# path to docs directory relative to top level of repository
# $GITHUB_WORKSPACE is set if the actions/checkout@v3 action is run first
# no op test

cwd=$(pwd)
DOCS_DIR="${GITHUB_WORKSPACE}/docs"

# run Make to build the documentation and return to previous directory
cd "${DOCS_DIR}"
make clean html
cd "${cwd}"

# copy HTML output into directory to create an artifact
mkdir -p artifact/documentation
cp -R "${DOCS_DIR}/build/html/." artifact/documentation

# check if the warnings.log file is empty
# Copy it into the artifact and documeentation directories
# so it will be available in the artifacts
warning_file="${DOCS_DIR}/build/warnings.log"
if [[ -s ${warning_file} ]]; then
  cp -r "${DOCS_DIR}/build/warnings.log" artifact/doc_warnings.log
  cp artifact/doc_warnings.log artifact/documentation
  echo "Warnings were encountered while building documentation."
  echo "========== Begin warnings =========="
  cat artifact/doc_warnings.log
  echo "=========== End warnings ==========="
fi
