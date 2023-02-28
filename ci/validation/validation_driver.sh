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
    echo "Running Automated Testing on $TARGET"
    source $MODULESHOME/init/sh
    source $my_dir/../${TARGET}.sh
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
# clone, checkout, build, test, etc.
repo_url="https://github.com/NOAA-EMC/GDASApp.git"
today=$(date +%Y%m%d)

mkdir -p $GDAS_CI_ROOT/validation/$today
cd $GDAS_CI_ROOT/validation/$today

# clone copy of repo
git clone $repo_url
cd GDASApp

# load modules
case ${TARGET} in
  hera | orion)
    echo "Loading modules on $TARGET"
    module purge
    module use $GDAS_CI_ROOT/validation/$today/GDASApp/modulefiles
    module load GDAS/$TARGET
    module list
    ;;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1
    ;;
esac

# run build and testing command
$my_dir/run_validation.sh -d $GDAS_CI_ROOT/validation/$today/GDASApp -o $GDAS_CI_ROOT/validation/$today/output.txt

# ==============================================================================
# scrub working directory for older files
find $GDAS_CI_ROOT/validation/* -maxdepth 1 -mtime +3 -exec rm -rf {} \;

