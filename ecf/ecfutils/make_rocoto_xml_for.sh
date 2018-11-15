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

if ( ! ( ( test -d /scratch4 && test -d /scratch3 )  || \
          ( test -d /usrx/local && ! test -e /etc/redhat-release ) || \
          test -d /lfs3 || \
          test -d /lustre/f1 || \
	  ( test -d /gpfs/dell2 && test -d /usrx && ( readlink /usrx | grep dell ) ) ) \
	      ) ; then
   echo "ERROR: This script only runs on WCOSS Cray, WCOSS Phase 3 (Dell), Jet, and Theia" 1>&2
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
