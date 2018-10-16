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
export FIRST_CYCLE="$2"
export LAST_CYCLE="$3"

if [[ ! -d /usrx/local ]] ; then
   echo "ERROR: This script only runs on WCOSS" 1>&2
   exit 1
fi

check_ecf_variables

if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then 
    echo "make_ecflow_files_for.sh: verbose mode"
fi

echo 'ecFlow server settings:'
echo "   port: $ECF_PORT"
echo "   root: $ECF_ROOT"
echo "   home: $ECF_HOME"
echo "   host: $ECF_HOST"

set +e
find_python36
set -e

if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then
    echo "make_ecflow_files_for.sh: EXPDIR=$EXPDIR"
    set -x
fi

# Profiling version:
# $python36 -c "import worktools, cProfile ; cmd='''worktools.make_ecflow_files_for_cycles(
#   '$EXPDIR',
#   '$FIRST_CYCLE',
#   '$LAST_CYCLE')''' ; cProfile.run(cmd) "

#$python36 -c "import worktools ; worktools.make_ecflow_files_for_cycles('$EXPDIR') "
$python36 -c "import worktools ; worktools.make_ecflow_files_for_cycles(
  '$EXPDIR',
  '$FIRST_CYCLE',
  '$LAST_CYCLE') "
