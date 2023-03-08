#!/bin/bash
set -eux

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
GH=/home/Terry.McGuinness/bin/gh
repo_url=${repo_url:-"https://github.com/global-workflow.git"}
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
current_date=$(date)
the_hostname=$(hostname)
{
 echo "Automated global-workflow Testing Results:"
 echo "Machine: ${TARGET}"
 echo '```'
 echo "Start: ${current_date} on ${the_hostname}"
 echo "---------------------------------------------------"
}  >> "${outfile}"
######################################################################

cd "${repodir}"
# clone copy of repo
if [[ -d global-workflow ]]; then
  rm -Rf global-workflow
fi

git clone "${repo_url}"
cd global-workflow

pr_state=$(gh pr view "${PR}" --json state --jq '.state')
if [[ "${pr_state}" != "OPEN" ]]; then
  title=$(gh pr view "${PR}" --json title --jq '.title')
  echo "PR ${title} is no longer open, state is ${pr_state} ... quitting"
  exit 1
fi  
 
# checkout pull request
"${GH}" pr checkout "${PR}" --repo "${repo_url}"

# get commit hash
commit=$(git log --pretty=format:'%h' -n 1)
echo "${commit}" > "../commit"

# run build script
cd sorc
export BUILD_JOBS=8
rm -rf log.build
./checkout.sh -g -c
# build full cycle
#./build_all.sh -g &>> log.build

# Validations
build_status=$?
if [[ ${build_status} -eq 0 ]]; then
{
  echo "Build:                                 *SUCCESS*"
  echo "Build: Completed at ${current_date}"
}  >> "${outfile}"
else
{
  echo "Build:                                  *FAILED*"
  echo "Build: Failed at ${current_date}"
  echo "Build: see output at ${PWD}/log.build"
}
  echo '```' >> "${outfile}"
fi

#./link_workflow.sh

cd ../ush/python/pygw
pip3 install .

echo "check/build/link test completed"
exit "${build_status}"
