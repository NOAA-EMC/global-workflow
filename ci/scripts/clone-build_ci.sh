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
  echo "  -p  PR nunber to clone and build"
  echo "  -d  Full path of <directory> of were to clone and build PR" 
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
      exit
     ;; 
  esac
done

####################################################################
# start output file
{
 echo "Automated global-workflow Testing Results:"
 echo "Machine: ${MACHINE_ID^}"
 echo '```'
 echo "Start: $(date) on $(hostname)" || true
 echo "---------------------------------------------------"
}  >> "${outfile}"
######################################################################

cd "${repodir}"
# clone copy of repo
if [[ -d global-workflow ]]; then
  rm -Rf global-workflow
fi

git clone "${REPO_URL}"
cd global-workflow

pr_state=$(gh pr view "${PR}" --json state --jq '.state')
if [[ "${pr_state}" != "OPEN" ]]; then
  title=$(gh pr view "${PR}" --json title --jq '.title')
  echo "PR ${title} is no longer open, state is ${pr_state} ... quitting"
  exit 1
fi  
 
# checkout pull request
"${GH}" pr checkout "${PR}" --repo "${REPO_URL}"

# get commit hash
commit=$(git log --pretty=format:'%h' -n 1)
echo "${commit}" > "../commit"

# run build script
cd sorc
export BUILD_JOBS=8
rm -rf log.build
./checkout.sh -c -g -u
# build full cycle
./build_all.sh  &>> log.build

# Validations
build_status=$?
if [[ ${build_status} != 0 ]]; then
  {
    echo "Build:                                  *FAILED*"
    echo "Build: Failed at $(date)" || true
    echo "Build: see output at ${PWD}/log.build"
  } echo '```' >> "${outfile}"
  exit "${build_status}"
else
  {
    echo "Build:                                 *SUCCESS*"
    echo "Build: Completed at $(date)" || true
  }  >> "${outfile}"
fi

./link_workflow.sh
build_status=$?

echo "check/build/link test completed"
exit "${build_status}"
