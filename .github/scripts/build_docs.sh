#! /bin/bash

# path to docs directory relative to top level of repository
# $GITHUB_WORKSPACE is set if the actions/checkout@v3 action is run first

cwd=$(pwd)
DOCS_DIR="${GITHUB_WORKSPACE}/docs"

# run Make to build the documentation and return to previous directory
cd "${DOCS_DIR}" || ( echo "unable to cd into ${DOCS_DIR}, ABORT!"; exit 1 )
make clean html
cd "${cwd}" || ( echo "unable to cd into ${cwd}, ABORT!"; exit 1 )

# copy HTML output into directory to create an artifact
mkdir -p artifact/documentation
cp -r "${DOCS_DIR}/_build/html/*" artifact/documentation

# check if the warnings.log file is empty
# Copy it into the artifact and documeentation directories
# so it will be available in the artifacts
warning_file="${DOCS_DIR}/_build/warnings.log"
if [[ -s ${warning_file} ]]; then
  cp -r "${DOCS_DIR}/_build/warnings.log" artifact/doc_warnings.log
  cp artifact/doc_warnings.log artifact/documentation
  exit 1
fi
