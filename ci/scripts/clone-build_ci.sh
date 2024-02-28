#!/bin/bash
set -eux

#####################################################################
#  Usage and arguments for specfifying cloned directgory
#####################################################################
usage() {
  set +x
  echo
  echo "Usage: $0 -p <PR#> -d <directory> -o <output> -h"
  echo
  echo "  -p  PR number to clone and build"
  echo "  -d  Full path of <directory> of where to clone and build PR"
  echo "  -o  Full path to output message file detailing results of CI tests"
  echo "  -h  display this message and quit"
  echo
  exit 1
}

################################################################
while getopts "p:d:o:h" opt; do
  case ${opt} in
    p)
      PR=${OPTARG}
      ;;
    d)
      repodir=${OPTARG}
      ;;
    o)
      outfile=${OPTARG}
      ;;
    h|\?|:)
      usage
      ;;
    *)
      echo "Unrecognized option"
      usage
     ;;
  esac
done

cd "${repodir}" || exit 1
if [[ -d global-workflow ]]; then
  rm -Rf global-workflow
fi

git clone "${REPO_URL}"
cd global-workflow || exit 1

# checkout pull request
"${GH}" pr checkout "${PR}" --repo "${REPO_URL}" --recurse-submodules
HOMEgfs="${PWD}"
source "${HOMEgfs}/ush/detect_machine.sh"

####################################################################
# start output file
{
 echo "Automated global-workflow Testing Results:"
 echo '```'
 echo "Machine: ${MACHINE_ID^}"
 echo "Start: $(date) on $(hostname)" || true
 echo "---------------------------------------------------"
}  >> "${outfile}"
######################################################################

# get commit hash
commit=$(git log --pretty=format:'%h' -n 1)
echo "${commit}" > "../commit"

# build full cycle
cd sorc || exit 1
set +e

source "${HOMEgfs}/ush/module-setup.sh"
export BUILD_JOBS=8
rm -rf log.build
./build_all.sh -guw  >> log.build 2>&1
build_status=$?

DATE=$(date +'%D %r')
if [[ ${build_status} != 0 ]]; then
  {
    echo "Build: *** FAILED ***"
    echo "Build: Failed at ${DATE}"
    cat "${PWD}/log.build"
  } >> "${outfile}"
  exit "${build_status}"
else
  {
    echo "Build: Completed at ${DATE}"
  } >> "${outfile}"
fi

LINK_LOGFILE_PATH=link_workflow.log
rm -f "${LINK_LOGFILE_PATH}"
./link_workflow.sh >> "${LINK_LOGFILE_PATH}" 2>&1
link_status=$?
if [[ ${link_status} != 0 ]]; then
  DATE=$(date +'%D %r')
  {
    echo "Link: *** FAILED ***"
    echo "Link: Failed at ${DATE}"
    cat "${LINK_LOGFILE_PATH}"
  } >> "${outfile}"
  exit "${link_status}"
fi

echo "check/build/link test completed"
exit "${build_status}"
