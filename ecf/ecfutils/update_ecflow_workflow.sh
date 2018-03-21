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

if [[ ! -d /usrx/local || -e /etc/redhat-release ]] ; then
   echo "ERROR: This script only runs on WCOSS Cray" 1>&2
   exit 1
fi

if ( ! which ecflow_client > /dev/null 2>&1 ) ; then
    echo "ERROR: There is no ecflow_client in your \$PATH.  Load the ecflow module."
    exit 1
fi

check_ecf_host=NO
check_ecf_variables

if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then 
    echo "begin_ecflow_workflow.sh: verbose mode"
    export redirect=" "
else
    export redirect="> /dev/null 2>&1"
fi

echo "ecFlow server port: $ECF_PORT"
echo "ecFlow server root: $ECF_ROOT"
echo "ecFlow server home: $ECF_HOME"

set +e
find_python36
set -e

if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then
    set -x
fi

if ( ! ecflow_client --ping $redirect ) ; then
    echo "Could not connect to ecflow server.  Aborting."
    exit 1
fi

$python36 -c "
import worktools ;
worktools.add_cycles_to_running_ecflow_workflow_at(
  '$EXPDIR',
  '$FIRST_CYCLE',
  '$LAST_CYCLE'
)"
