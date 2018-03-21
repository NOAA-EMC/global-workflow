#! /bin/bash

set -ue

# Get the directory in which this script resides.  We'll assume the
# yaml files are there:
dir0=$( dirname "$0" )
here=$( cd "$dir0" ; pwd -P )

if [[ ! -s .in-the-ecfutils-dir ]] ; then
    echo "This script must be within the ecf/ecfutils directory when running it." 1>&2
    exit 2
fi

export WORKTOOLS_VERBOSE=NO

crowdir=$( cd CROW ; pwd -P )

# Make sure this directory is in the python path so we find worktools.py:
export PYTHONPATH=$here:$crowdir:${PYTHONPATH:+:$PYTHONPATH}

source "$dir0/worktools.sh.inc"

find_python36

$python36 -c '
import sys;
import worktools;
worktools.setup_case(sys.argv[1:])' \
     "$@"
