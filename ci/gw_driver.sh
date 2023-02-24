#!/bin/bash --login

my_dir="$( cd "$( dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd )"

# ==============================================================================
usage() {
  set +x
  echo
  echo "Usage: $0 -t <target> -h"
  echo
  echo "  -t  target/machine script is running on    DEFAULT: $(hostname)"
  echo "  -h  display this message and quit"
  echo
  exit 1
}

# ==============================================================================
# First, set up runtime environment

export TARGET="$(hostname)"

while getopts "t:h" opt; do
  case $opt in
    t)
      TARGET=$OPTARG
      ;;
    h|\?|:)
      usage
      ;;
  esac
done

case ${TARGET} in
  hera | orion)
    echo "Running automated testing with workflow on $TARGET"
    source $MODULESHOME/init/sh
    source $my_dir/${TARGET}.sh
    module purge
    module use $GDAS_MODULE_USE
    module load GDAS/$TARGET
    module list
    ;;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1
    ;;
esac

# ==============================================================================
# pull on the repo and get list of open PRs
cd $GDAS_CI_ROOT/repo
CI_LABEL="${GDAS_CI_HOST}-GW-RT"
gh pr list --label "$CI_LABEL" --state "open" | awk '{print $1;}' > $GDAS_CI_ROOT/open_pr_list_gw
open_pr_list=$(cat $GDAS_CI_ROOT/open_pr_list_gw)

# ==============================================================================
# clone, checkout, build, test, etc.
repo_url="https://github.com/NOAA-EMC/GDASApp.git"
workflow_url="https://github.com/NOAA-EMC/global-workflow.git"
# loop through all open PRs
for pr in $open_pr_list; do
  gh pr edit $pr --remove-label $CI_LABEL --add-label ${CI_LABEL}-Running
  echo "Processing Pull Request #${pr}"
  mkdir -p $GDAS_CI_ROOT/workflow/PR/$pr
  cd $GDAS_CI_ROOT/workflow/PR/$pr

  # clone global workflow develop branch
  git clone $workflow_url

  # run checkout script for all other components
  cd $GDAS_CI_ROOT/workflow/PR/$pr/global-workflow/sorc
  ./checkout.sh -u

  # checkout pull request
  cd gdas.cd
  git checkout develop
  git pull
  gh pr checkout $pr

  # get commit hash
  commit=$(git log --pretty=format:'%h' -n 1)
  echo "$commit" > $GDAS_CI_ROOT/workflow/PR/$pr/commit

  $my_dir/run_gw_ci.sh -d $GDAS_CI_ROOT/workflow/PR/$pr/global-workflow -o $GDAS_CI_ROOT/workflow/PR/$pr/output_${commit}
  ci_status=$?
  gh pr comment $pr --body-file $GDAS_CI_ROOT/workflow/PR/$pr/output_${commit}
  if [ $ci_status -eq 0 ]; then
    gh pr edit $pr --remove-label ${CI_LABEL}-Running --add-label ${CI_LABEL}-Passed
  else
    gh pr edit $pr --remove-label ${CI_LABEL}-Running --add-label ${CI_LABEL}-Failed
  fi
done

# ==============================================================================
# scrub working directory for older files
find $GDAS_CI_ROOT/workflow/PR/* -maxdepth 1 -mtime +3 -exec rm -rf {} \;

