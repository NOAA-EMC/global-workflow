#! /bin/bash

set -ue

# Get the directory in which this script resides.  We'll assume the
# yaml files are there:
dir0=$( dirname "$0" )
here=$( cd "$dir0" ; pwd -P )

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
export CONFIGDIR="$1"
export FIRST_CYCLE="$2"
export LAST_CYCLE="$3"

if [[ ! -d /usrx/local || -e /etc/redhat-release ]] ; then
   echo "ERROR: This script only runs on WCOSS Cray" 1>&2
   exit 1
fi

check_ecf_variables

if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then 
    echo "remake_ecflow_files_for.sh: verbose mode"
fi

echo 'ecFlow server settings:'
echo "   port: $ECF_PORT"
echo "   root: $ECF_ROOT"
echo "   home: $ECF_HOME"
echo "   host: $ECF_HOST"

set +e
find_python36
set -e

tmpfile=${TMPDIR:-/tmp}/find-expdir.$RANDOM.$RANDOM.$$

if ( ! ( make_yaml_files ) ) ; then
    echo "Failed to make YAML files"
    exit 1
fi

source "$tmpfile"
rm -f "$tmpfile"

if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then
    echo "remake_ecflow_files_for.sh: EXPDIR=$EXPDIR"
    set -x
fi

$python36 -c "import worktools ; worktools.remake_ecflow_files_for_cycles(
  '$EXPDIR',
  '$FIRST_CYCLE',
  '$LAST_CYCLE')"
