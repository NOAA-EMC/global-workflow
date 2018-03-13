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

# Parse arguments:
if [[ "$1" == "-v" ]] ; then
    export WORKTOOLS_VERBOSE=YES
    shift 1
fi
export EXPDIR="$1"

if [[ ! ( -d /scratch4 && -d /scratch3 || \
          -d /usrx/local && ! -e /etc/redhat-release || \
          -d /lfs3 ) \
    ]] ; then
   echo "ERROR: This script only runs on WCOSS Cray, Jet, and Theia" 1>&2
   exit 1
fi

set +e
find_python36
set -e

if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then
    echo "make_rocoto_xml_for.sh: EXPDIR=$EXPDIR"
    set -x
fi

$python36 -c "import worktools ; worktools.make_rocoto_xml_for(
  '$EXPDIR')"
