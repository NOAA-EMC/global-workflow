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
"${GH}" pr checkout "${PR}" --repo "${REPO_URL}"
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

# run checkout script
cd sorc || exit 1
set +e
./checkout.sh -c -g -u >> log.checkout 2>&1
checkout_status=$?
if [[ ${checkout_status} != 0 ]]; then
  {
    echo "Checkout: *** FAILED ***"
    echo "Checkout: Failed at $(date)" || true
    echo "Checkout: see output at ${PWD}/log.checkout"
  } >> "${outfile}"
  exit "${checkout_status}"
else
  {
    echo "Checkout: Completed at $(date)" || true
  } >> "${outfile}"
fi

# build full cycle
source "${HOMEgfs}/ush/module-setup.sh"
export BUILD_JOBS=8
rm -rf log.build
./build_all.sh  >> log.build 2>&1
build_status=$?

if [[ ${build_status} != 0 ]]; then
  {
    echo "Build: *** FAILED ***"
    echo "Build: Failed at $(date)" || true
    echo "Build: see output at ${PWD}/log.build"
  } >> "${outfile}"
  exit "${build_status}"
else
  {
    echo "Build: Completed at $(date)" || true
  } >> "${outfile}"
fi

./link_workflow.sh
link_status=$?
if [[ ${link_status} != 0 ]]; then
  {
    echo "Link: *** FAILED ***"
    echo "Link: Failed at $(date)" || true
  } >> "${outfile}"
  exit "${link_status}"
fi

echo "check/build/link test completed"
exit "${build_status}"
