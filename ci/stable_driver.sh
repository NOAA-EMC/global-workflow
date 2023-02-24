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
    echo "Running stability check on $TARGET"
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
# clone a fresh copy of the develop branch
datestr="$(date +%Y%m%d)"
repo_url="https://github.com/NOAA-EMC/GDASApp.git"
stableroot=$GDAS_CI_ROOT/stable

mkdir -p $stableroot/$datestr
cd $stableroot/$datestr
git clone $repo_url
cd GDASApp

# ==============================================================================
# run ecbuild to get the repos cloned
mkdir -p build
cd build
ecbuild ../
cd ..
rm -rf build

# ==============================================================================
# update the hashes to the most recent
$my_dir/stable_mark.sh $stableroot/$datestr/GDASApp

# ==============================================================================
# run the automated testing
$my_dir/run_ci.sh -d $stableroot/$datestr/GDASApp -o $stableroot/$datestr/output
ci_status=$?
if [ $ci_status -eq 0 ]; then
  # push a new commit to the stable branch
  cd $stableroot/$datestr/GDASApp
  git push origin --delete feature/stable-nightly
  git checkout -b feature/stable-nightly
  git add CMakeLists.txt
  git commit -m "Update to new stable build on $datestr"
  git push --set-upstream origin feature/stable-nightly
  echo "Stable branch updated"
else
  # do nothing
  echo "Testing failed, stable branch will not be updated"
fi
# ==============================================================================
# publish some information to RZDM for quick viewing
# THIS IS A TODO FOR NOW

# ==============================================================================
# scrub working directory for older files
find $stableroot/* -maxdepth 1 -mtime +3 -exec rm -rf {} \;
