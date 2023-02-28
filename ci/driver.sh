#!/bin/bash --login

set -ex

GH_EXEC=/home/Terry.McGuinness/bin/gh
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
    echo "Running Automated Testing on $TARGET"
    source $my_dir/${TARGET}.sh
    ;;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1
    ;;
esac

#repo_url="https://github.com/NOAA-EMC/global-workflow.git"
# using Terrence.McGuinness-NOAA for development with GitHub interations
repo_url="https://github.com/TerrenceMcGuinness-NOAA/global-workflow.git"

# ==============================================================================
# pull on the repo and get list of open PRs
mkdir -p $GFS_CI_ROOT/repo
cd $GFS_CI_ROOT/repo
if [ ! -d $GFS_CI_ROOT/repo/global-workflow ]; then
 git clone $repo_url
fi
cd $GFS_CI_ROOT/repo/global-workflow
git pull
CI_LABEL="${GFS_CI_HOST}-GW-RT"
$GH_EXEC pr list --label "$CI_LABEL" --state "open" | awk '{print $1;}' > $GFS_CI_ROOT/open_pr_list
if [ -s $GFS_CI_ROOT/open_pr_list ]; then
 open_pr_list=$(cat $GFS_CI_ROOT/open_pr_list)
else
 echo "no PRs to process .. exit"
 exit
fi 

# ==============================================================================
# clone, checkout, build, test, etc.
# loop throu all open PRs
for pr in $open_pr_list; do
  $GH_EXEC pr edit --repo $repo_url $pr --remove-label $CI_LABEL --add-label ${CI_LABEL}-Running
  echo "Processing Pull Request #${pr}"
  mkdir -p $GFS_CI_ROOT/PR/$pr
  cd $GFS_CI_ROOT/PR/$pr

  # clone copy of repo
  git clone $repo_url
  cd global-workflow

  # checkout pull request
  $GH_EXEC pr checkout $pr --repo $repo_url

  # get commit hash
  commit=$(git log --pretty=format:'%h' -n 1)
  echo "$commit" > $GFS_CI_ROOT/PR/$pr/commit

  # run build and testing command
  $my_dir/run_ci.sh -d $GFS_CI_ROOT/PR/$pr/global-workflow -o $GFS_CI_ROOT/PR/$pr/output_${commit}
  ci_status=$?
  $GH_EXEC pr comment $pr --repo $repo_url --body-file $GFS_CI_ROOT/PR/$pr/output_${commit}
  if [ $ci_status -eq 0 ]; then
    $GH_EXEC pr edit --repo $repo_url $pr --remove-label ${CI_LABEL}-Running --add-label ${CI_LABEL}-Passed
  else
    $GH_EXEC pr edit $pr --repo $repo_url --remove-label ${CI_LABEL}-Running --add-label ${CI_LABEL}-Failed
  fi
done

# ==============================================================================
# scrub working directory for older files
find $GFS_CI_ROOT/PR/* -maxdepth 1 -mtime +3 -exec rm -rf {} \;
